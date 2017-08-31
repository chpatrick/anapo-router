{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Foldable
import Data.Monoid
import GHC.Generics
import qualified Data.JSString as JSString
import qualified Data.Text as T

import Anapo.ClientM
import Anapo.Component
import Anapo.Loop
import Anapo.Render
import Anapo.Router

data NavigationItem
  = Home
  | About
  | Team
    deriving (Eq, Ord)

navItemPath :: NavigationItem -> Path
navItemPath = \case
  Home -> "/"
  About -> "/about"
  Team -> "/team"

navItemTitle :: NavigationItem -> T.Text
navItemTitle = \case
  Home -> "Home"
  About -> "About"
  Team -> "Team"

navigation :: History -> NavigationItem -> Component' state
navigation history currentItem =
  n$ nav_
    (class_ "navbar navbar-expand-lg navbar-light bg-light") $ do
      n$ historyLinkA history (navItemPath Home)
        (class_ "navbar-brand") $
          n$ "Anapo Router"

      n$ div_ $
          n$ ul_
            (class_ "navbar-nav") $ do
              for_ [ Home, About, Team ] $ \navItem ->
                n$ li_
                  (class_ (if navItem == currentItem then "nav-item active" else "nav-item")) $
                    n$ historyLinkA history (navItemPath navItem)
                      (class_ "nav-link") $
                        n$ text (navItemTitle navItem)

data TeamRoutes = TeamRoutes
  { teamDetail :: Capture JSString.JSString :> End ()
  , teamHome :: End ()
  } deriving (Generic)

instance Router TeamRoutes where
  type RouteState TeamRoutes = ()

data TestRoutes = TestRoutes
  { home :: End ()
  , about :: "about" /> End ()
  , team :: "team" /> Wrap :> TeamRoutes
  } deriving (Generic)

instance Router TestRoutes where
  type RouteState TestRoutes = ()

data TeamMember = TeamMember
  { tmName :: T.Text
  , tmRole :: T.Text
  }

teamMembers :: [ ( JSString.JSString, TeamMember ) ]
teamMembers =
  [ ( "gordon", TeamMember "Gordon Cole" "Director" )
  , ( "albert", TeamMember "Albert Rosenfield" "Agent" )
  , ( "tammy", TeamMember "Tammy Preston" "Agent" )
  ]

teamRoutes :: TeamRoutes
teamRoutes = TeamRoutes
  { teamHome = brief $ do
      n$ "Pick a team member!"

  , teamDetail = brief $ \teamMemberId -> do
      case lookup teamMemberId teamMembers of
        Nothing -> n$ "Unknown team member."
        Just teamMember -> do
          n$ h2_ $
            n$ text (tmName teamMember)

          n$ span_ $
            n$ text (tmRole teamMember)
  }

testRoutes :: History -> TestRoutes
testRoutes history = TestRoutes
  { home = brief $ do
      navigation history Home
      n$ "Hello world!"

  , about = brief $ do
      navigation history About
      n$ "About."

  , team = brief $ Wrap teamRoutes $ \inner -> do
      navigation history Team

      n$ div_
        (class_ "row") $ do
          n$ div_
            (class_ "col-4") $
              n$ div_
                (class_ "list-group") $
                  for_ teamMembers $ \( memberId, teamMember ) ->
                    n$ historyLinkA history ("/team/" <> memberId)
                      (class_ "list-group-item list-group-item-action") $
                        n$ text (tmName teamMember)

          n$ div_
            (class_ "col-8") $
            inner
  }

main :: IO ()
main = runClientM $ withBrowserHistory $ \history -> do
  st <- routerInit ()
  let rootComponent = routerComponent history (testRoutes history)
  installComponentBootstrap RenderOptions{roAlwaysRerender = False, roDebugOutput = True} st rootComponent
