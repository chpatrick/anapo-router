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
  , PathIn(..)
  , Router(..)
  , Abbreviated(..)
  , HasLink(..)
  , fieldLink
  , rootLink
  , toPath

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
import Data.Monoid
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

newtype PathIn in_ = PathIn PathSegments

toPath :: PathIn in_ -> Path
toPath (PathIn segs) = mconcat $ map ("/"<>) segs

class Router r where
  type RouteState r :: *
  tryRoute :: PathSegments -> Maybe (r -> RouteComponent r)
  default tryRoute :: (Generic r, Router (Rep r ()), RouteState (Rep r ()) ~ RouteState r) => PathSegments -> Maybe (r -> RouteComponent r)
  tryRoute = tryRouteNext (\router -> GHC.Generics.from router :: Rep r ())

class HasLink r where
  type MkLink in_ r :: *
  type instance MkLink in_ r = PathIn r -> PathIn in_
  mkLink :: proxy r -> proxy' in_ -> Endo PathSegments -> MkLink in_ r
  default mkLink :: (MkLink in_ r ~ (PathIn r -> PathIn in_)) => proxy r -> proxy' in_ -> Endo PathSegments -> MkLink in_ r
  mkLink _ _ segs (PathIn path) = PathIn (appEndo segs [] ++ path)

mkLinkNext :: forall r next in_ proxy proxy'. HasLink next => proxy (r next) -> proxy' in_ -> Endo PathSegments -> MkLink in_ next
mkLinkNext _ = mkLink (Proxy :: Proxy next)

fieldLink :: forall r a. HasLink a => (r -> a) -> MkLink r a
fieldLink _ = mkLink (Proxy :: Proxy a) (Proxy :: Proxy r) mempty

rootLink :: forall a proxy. HasLink a => proxy a -> MkLink () a
rootLink _ = mkLink (Proxy :: Proxy a) (Proxy :: Proxy ()) mempty

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

instance HasLink (End state) where
  type MkLink in_ (End state) = PathIn in_
  mkLink _ _ segs = PathIn (appEndo segs [])

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

instance HasLink next => HasLink (Wrap next) where
  type MkLink in_ (Wrap next) = MkLink in_ next
  mkLink = mkLinkNext

instance Abbreviated (Wrap state)

newtype Seg (seg :: Symbol) next = Seg { segNext :: next }

instance (KnownSymbol seg, Router next) => Router (Seg seg next) where
  type RouteState (Seg seg next) = RouteState next
  tryRoute (s : nextPath)
    | JSString.unpack s == symbolVal (Proxy :: Proxy seg) = tryRouteNext segNext nextPath
  tryRoute _ = Nothing

instance (KnownSymbol seg, HasLink next) => HasLink (Seg seg next) where
  type MkLink in_ (Seg seg next) = MkLink in_ next
  mkLink pr pin_ segs = mkLinkNext pr pin_ (segs <> Endo (seg:))
    where
      seg = JSString.pack (symbolVal (Proxy :: Proxy seg))

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

instance HasLink next => HasLink (ZoomR out next) where
  type MkLink in_ (ZoomR out next) = MkLink in_ next
  mkLink = mkLinkNext

instance Abbreviated next => Abbreviated (ZoomR out next)

class FromSegment a where
  fromSegment :: JSString.JSString -> Maybe a

class ToSegment a where
  toSegment :: a -> JSString.JSString

instance FromSegment JSString.JSString where
  fromSegment = Just

instance ToSegment JSString.JSString where
  toSegment = id

newtype Capture a next = Capture { captureNext :: a -> next }

instance (FromSegment a, Router next) => Router (Capture a next) where
  type RouteState (Capture a next) = RouteState next
  tryRoute (s : segs)
    | Just segVal <- fromSegment s = tryRouteNext (\(Capture next) -> next segVal) segs
  tryRoute _ = Nothing

instance (ToSegment a, HasLink next) => HasLink (Capture a next) where
  type MkLink in_ (Capture a next) = a -> MkLink in_ next
  mkLink pr pin_ segs segVal = mkLinkNext pr pin_ (segs <> Endo (toSegment segVal:))

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
