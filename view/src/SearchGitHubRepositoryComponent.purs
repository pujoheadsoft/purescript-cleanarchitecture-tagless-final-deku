module SearchGitHubRepositoryComponent where

import Prelude

import Control.Alt ((<|>))
import Controller.GitHubRepositoryController (searchRepositoryByName)
import Data.Either (Either(..))
import Data.Foldable (for_, oneOfMap, traverse_)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Tuple.Nested ((/\))
import Deku.Control (text_)
import Deku.Core (Nut(..), dyn, fixed)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.DOM.Self as Self
import Deku.Do as Deku
import Deku.Hooks (useDynAtBeginning, useRef, useState, useState', (<#~>))
import Deku.Pursx ((~~))
import Deku.Toplevel (runInBody')
import Effect (Effect)
import FRP.Event (Event)
import FRP.Poll (APoll)
import FRP.Poll as P
import Network.RemoteData (isLoading)
import State.SearchGitHubRepositoryState (ErrorMessage, SearchGitHubRepositoryState, GitHubRepositories)
import Type.Proxy (Proxy(..))
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.HTML.Window (alert)
import Web.UIEvent.KeyboardEvent (code, toEvent)

--   HH.form
--     [ HE.onSubmit SearchRepository ]
--     [ HH.h1_ [ HH.text "Search GitHub Repository" ]
--     , HH.label_
--         [ HH.div_ [ HH.text "Enter repository name:" ]
--         , HH.input
--             [ HP.value state.searchRepositoryName
--             , HE.onValueInput SetSearchRepositoryName
--             ]
--         ]
--     , HH.button
--         [ HP.disabled $ state.isLoading
--         , HP.type_ HP.ButtonSubmit
--         ]
--         [ HH.text "Search" ]
--     , renderRepositories state.repositories
--     ]
component_ = Proxy :: Proxy """
<div>
  <h1>Search GitHub Repository</h1>
  ~formMatter~
  ~result~
</div>
"""

component
  :: (APoll Event (Either ErrorMessage GitHubRepositories))
  -> (APoll Event Boolean)
  -> (String -> Effect Unit)
  -> Nut
component repositories isLoading onChangeName = component_ ~~
  { formMatter: fixed 
    [ Deku.do
        setName /\ name <- useState ""
        ref <- useRef "" name
        D.div_
          [ D.label_ 
              [ D.div_ [ D.text_ "Enter repository name:" ]
              , D.input
                  [ DA.xtypeText
                  , DA.value name
                  , DL.valueOn_ DL.change setName ]
                  []
              , D.button
                  [ DL.click_ \_ -> ref >>= onChangeName
                  , DA.disabled $ isLoading <#> show ]
                  [ D.text_ "Search" ]
              ]
          ]
    ]
  , result: fixed
    [ repositories <#~> renderRepositories ]
  }
  where
  renderRepositories = case _ of
    Left err ->
      D.div_ [ D.text_ err ] -- $ "Failed loading repositories: " <>
    Right r ->
      D.div_ (renderRepository <$> r)

  renderRepository repository =
    D.div
      [  ] 
      [ D.div [] [D.text_ repository.owner]
      , D.div [] [D.a [DA.href_ repository.url ] [ D.text_ repository.name ]]
      , D.div [] [D.text_ repository.updateDate]
      ]

-- render :: forall m. SearchGitHubRepositoryState -> H.ComponentHTML Action () m
-- render state =
--   HH.form
--     [ HE.onSubmit SearchRepository ]
--     [ HH.h1_ [ HH.text "Search GitHub Repository" ]
--     , HH.label_
--         [ HH.div_ [ HH.text "Enter repository name:" ]
--         , HH.input
--             [ HP.value state.searchRepositoryName
--             , HE.onValueInput SetSearchRepositoryName
--             ]
--         ]
--     , HH.button
--         [ HP.disabled $ state.isLoading
--         , HP.type_ HP.ButtonSubmit
--         ]
--         [ HH.text "Search" ]
--     , renderRepositories state.repositories
--     ]
--   where
--   renderRepositories = case _ of
--     Left err ->
--       HH.div_ [ HH.text $ "Failed loading repositories: " <> err ]
--     Right repositories ->
--       HH.div_ (renderRepository <$> repositories)
  
--   renderRepository repository =
--     HH.div
--       [ styleContainer ] 
--       [ HH.div [styleOwner] [HH.text repository.owner]
--       , HH.div [styleUrl] [HH.a [HP.href repository.url ] [ HH.text repository.name ]]
--       , HH.div [styleUpdateDate] [HH.text repository.updateDate]
--       ]
--   styleContainer = HP.style "display: flex; column-gap: 8px;"

-- data Action
--   = SetSearchRepositoryName String
--   | SearchRepository Event

-- component :: forall q i o m. MonadAff m => H.Component q i o m
-- component =
--   H.mkComponent
--     { initialState
--     , render
--     , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
--     }

-- initialState :: forall i. i -> SearchGitHubRepositoryState
-- initialState _ = { searchRepositoryName: mempty, repositories: Right mempty, isLoading: false }



--   styleOwner = CSS.style do
--     width $ px 150.0
--     overflow hidden
--     textWhitespace whitespaceNoWrap
--     textOverflow ellipsis

--   styleUrl = CSS.style do
--     width $ px 350.0
--     overflow hidden
--     textWhitespace whitespaceNoWrap
--     textOverflow ellipsis

--   styleUpdateDate = CSS.style do 
--     width $ px 100.0

-- handleAction :: forall o m. MonadAff m => Action -> H.HalogenM SearchGitHubRepositoryState Action () o m Unit
-- handleAction = case _ of
--   SetSearchRepositoryName searchRepositoryName -> do
--     H.modify_ (_ { searchRepositoryName = searchRepositoryName })

--   SearchRepository event -> do
--     H.liftEffect $ Event.preventDefault event
--     searchRepositoryByName =<< H.gets _.searchRepositoryName