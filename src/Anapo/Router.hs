{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Anapo.Router
  ( History(..)
  , withBrowserHistory
  , RouterState()
  , routerInit
  , routerComponent

  , historyLink
  , historyLinkA

  , Path
  , PathSegments
  , Router(..)
  , Abbreviated(..)
  , End(..)
  , Seg(..)
  , Capture(..)
  , Wrap(..)
  , ZoomR(..)
  , (:>)
  , type (/>)
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Proxy
import GHC.Generics
import GHC.TypeLits
import qualified Data.JSString as JSString
import qualified Data.JSString.Text as JSString
import qualified Data.HashMap.Strict as HMS
import GHCJS.Foreign

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Event as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM hiding (preventDefault)
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import qualified GHCJS.DOM.Location as DOM
import qualified GHCJS.DOM.Window as DOM
import qualified GHCJS.DOM.History as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.HTMLAnchorElement as DOM

import Anapo.ClientM
import Anapo.Component
import qualified Anapo.VDOM as V

type Path = JSString.JSString
type PathSegments= [ JSString.JSString ]

type RouteComponent r = Component' (RouteState r)

class Router r where
  type RouteState r :: *
  tryRoute :: PathSegments -> Maybe (r -> RouteComponent r)
  default tryRoute :: (Generic r, Router (Rep r ()), RouteState (Rep r ()) ~ RouteState r) => PathSegments -> Maybe (r -> RouteComponent r)
  tryRoute = tryRouteNext (\router -> GHC.Generics.from router :: Rep r ())

type a :> b = a b
infixr 2 :>

type seg /> b = Seg seg b
infixr 2 />

tryRouteNext :: Router r' => (r -> r') -> PathSegments -> Maybe (r -> RouteComponent r')
tryRouteNext f segs = do
  routeNext <- tryRoute segs
  return (routeNext . f)

class Abbreviated a where
  type Brief a :: *
  type instance Brief a = a
  brief :: Brief a -> a
  default brief :: a -> a
  brief = id

newtype End state = End { endComponent :: Component' state }

instance Router (End state) where
  type RouteState (End state) = state
  tryRoute segs = endComponent <$ guard (null segs)

instance Abbreviated (End state) where
  type Brief (End state) = Component' state
  brief = End

data Wrap next = Wrap
  { wrapNext :: next
  , wrapWith :: RouteComponent next -> RouteComponent next
  }

instance Router next => Router (Wrap next) where
  type RouteState (Wrap next) = RouteState next
  tryRoute segs = do
    routeNext <- tryRoute segs
    return $ \(Wrap next f) -> f (routeNext next)

instance Abbreviated (Wrap state)

newtype Seg (seg :: Symbol) next = Seg { segNext :: next }

instance (KnownSymbol seg, Router next) => Router (Seg seg next) where
  type RouteState (Seg seg next) = RouteState next
  tryRoute (s : nextPath)
    | JSString.unpack s == symbolVal (Proxy :: Proxy seg) = tryRouteNext segNext nextPath
  tryRoute _ = Nothing

instance Abbreviated next => Abbreviated (Seg seg next) where
  type Brief (Seg seg next) = Brief next
  brief = Seg . brief

data ZoomR out next = ZoomR
  { zoomLens :: Lens' out (RouteState next)
  , zoomNext :: next
  }

instance Router next => Router (ZoomR out next) where
  type RouteState (ZoomR out next) = out
  tryRoute segs = do
    routeNext <- tryRoute segs
    return $ \(ZoomR l next) -> zoom' l (routeNext next)

instance Abbreviated next => Abbreviated (ZoomR out next)

class FromSegment a where
  fromSegment :: JSString.JSString -> Maybe a

instance FromSegment JSString.JSString where
  fromSegment = Just

newtype Capture a next = Capture { captureNext :: a -> next }

instance (FromSegment a, Router next) => Router (Capture a next) where
  type RouteState (Capture a next) = RouteState next
  tryRoute (s : segs)
    | Just segVal <- fromSegment s = tryRouteNext (\(Capture next) -> next segVal) segs
  tryRoute _ = Nothing

instance Abbreviated next => Abbreviated (Capture a next) where
  type Brief (Capture a next) = a -> Brief next
  brief x = Capture (brief . x)

instance Router r => Router (K1 i r p) where
  type RouteState (K1 i r p) = RouteState r
  tryRoute = tryRouteNext unK1

instance Router (f p) => Router (M1 i c f p) where
  type RouteState (M1 i c f p) = RouteState (f p)
  tryRoute = tryRouteNext unM1

instance (Router (left p), Router (right p), RouteState (left p) ~ RouteState (right p)) => Router ((left :*: right) p) where
  type RouteState ((left :*: right) p) = RouteState (left p)
  tryRoute segs = routeLeft <|> routeRight
    where
      routeLeft = tryRouteNext (\(left :*: _) -> left) segs
      routeRight = tryRouteNext (\(_ :*: right) -> right) segs

data History = History
  { historyPush :: Path -> ClientM ()
  , historyAddListener :: (Path -> ClientM ()) -> ClientM (ClientM ())
  , historyGetPathname :: ClientM Path
  }

data Listeners = Listeners Int (HMS.HashMap Int (JSString.JSString -> ClientM ()))

historyLink :: (DOM.IsElement el, DOM.IsGlobalEventHandlers el) => History -> Path -> V.SomeEvent el
historyLink history path = onclick_ $ \_ ev -> do
  DOM.preventDefault ev
  historyPush history path

historyLinkA :: (ConstructElement DOM.HTMLAnchorElement a) => History -> Path -> a
historyLinkA history path =
  a_
    (href_ (JSString.textFromJSString path))
    (historyLink history path)

withBrowserHistory :: (MonadMask m, MonadIO m) => (History -> m a) -> m a
withBrowserHistory cont = bracket setup destroy (cont . fst)
  where
    setup = liftIO $ do
      listenersVar <- newMVar (Listeners 0 HMS.empty)

      window <- DOM.currentWindowUnchecked

      domHistory <- DOM.getHistory window
      domLocation <- DOM.getLocation window

      let notifyListeners newPath = do
            Listeners _ listeners <- readMVar listenersVar
            for_ listeners $ \listener -> listener newPath

      let history = History
            { historyPush = \path -> do
                DOM.pushState domHistory jsNull ("" :: JSString.JSString) (Just path)
                notifyListeners path

            , historyAddListener = \listener -> do
                addedIndex <- modifyMVar listenersVar $
                  \(Listeners newIndex listeners) ->
                    return ( Listeners (newIndex + 1) (HMS.insert newIndex listener listeners), newIndex )

                return $ modifyMVar_ listenersVar $
                  \(Listeners newIndex listeners) ->
                    return (Listeners newIndex (HMS.delete addedIndex listeners))

            , historyGetPathname = DOM.getPathname domLocation
            }

      releaseOnPopState <- DOM.on window DOM.popState $ liftIO $ do
        path <- DOM.getPathname domLocation
        notifyListeners path

      return ( history, releaseOnPopState )

    destroy ( _, cleanup ) = liftIO cleanup

data RouterState state = RouterState
  { _routerMountState :: IORef (Maybe (IO ()))
  , _routerCurrentComponent :: Maybe (Component' state)
  , _routerInnerState :: state
  }
makeLenses ''RouterState

routerInit :: state -> ClientM (RouterState state)
routerInit initState = do
  mountState <- newIORef Nothing
  return RouterState
    { _routerMountState = mountState
    , _routerCurrentComponent = Nothing
    , _routerInnerState = initState
    }

routerComponent :: Router r => History -> r -> Component' (RouterState (RouteState r))
routerComponent history router = do
  dispatch <- askDispatch
  st <- askState

  let updateRoute path = do
        let segs = dropWhile JSString.null $ JSString.splitOn "/" path
        putStrLn ("segs: " ++ show segs)
        let mbComponent = fmap ($ router) $ tryRoute segs
        putStrLn ("mbComponent: " ++ show (isJust mbComponent))
        dispatch (set routerCurrentComponent mbComponent)

  let setup _ = do
        releaseHistoryListener <- historyAddListener history updateRoute

        writeIORef (st ^. routerMountState) (Just releaseHistoryListener)
        initPath <- historyGetPathname history
        updateRoute initPath

  let cleanup _ = do
        mbCleanup <- readIORef (st ^. routerMountState)
        sequence_ mbCleanup

  n$ do
    wrapper <- div_ $
      forOf_ (routerCurrentComponent . folded) st $ zoom' routerInnerState

    return $ wrapper
      { V.nodeCallbacks = mempty
        { V.callbacksUnsafeDidMount = setup
        , V.callbacksUnsafeWillRemove = cleanup
        }
      }
