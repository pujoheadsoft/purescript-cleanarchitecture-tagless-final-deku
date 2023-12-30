module SearchGitHubRepositoryComponent where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useRef, useState, useState', (<#~>))
import Deku.Pursx ((~~))
import Domain.GitHubRepository (GitHubRepositoryName(..))
import Driver.GitHubApiDriver (gitHubRepositoryGatewayPortFunction)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gateway.GitHubRepositoryGateway (gitHubRepositoryPortFunction)
import Presenter.GitHubRepositoryPresenter (gitHubRepositoryPresenterPortFunction)
import Record.Builder (build, merge)
import Type.Proxy (Proxy(..))
import UseCase.SearchGitHubRepositoryUseCase (execute)

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

component :: Nut
component = Deku.do 
  setRepositories /\ repositories <- useState'
  setLoading /\ isLoading <- useState'

  let
    gf = gitHubRepositoryPortFunction gitHubRepositoryGatewayPortFunction
    pf = gitHubRepositoryPresenterPortFunction {
      setRepositories: \r -> liftEffect $ setRepositories (Right r),
      setLoading: \loading -> liftEffect $ setLoading loading,
      setErrorMessage: \m -> liftEffect $ setRepositories (Left m)
    } 
    functions = build (merge (gf)) pf

    searchRepositoryByName name = runReaderT (execute (GitHubRepositoryName name)) functions
  
  component_ ~~ { 
    formMatter: fixed 
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
                  [ DL.click_ \_ -> ref >>= searchRepositoryByName >>> launchAff_
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



-- styleOwner = CSS.style do
--   width $ px 150.0
--   overflow hidden
--   textWhitespace whitespaceNoWrap
--   textOverflow ellipsis

-- styleUrl = CSS.style do
--   width $ px 350.0
--   overflow hidden
--   textWhitespace whitespaceNoWrap
--   textOverflow ellipsis

-- styleUpdateDate = CSS.style do 
--   width $ px 100.0

-- handleAction :: forall o m. MonadAff m => Action -> H.HalogenM SearchGitHubRepositoryState Action () o m Unit
-- handleAction = case _ of
--   SetSearchRepositoryName searchRepositoryName -> do
--     H.modify_ (_ { searchRepositoryName = searchRepositoryName })

--   SearchRepository event -> do
--     H.liftEffect $ Event.preventDefault event
--     searchRepositoryByName =<< H.gets _.searchRepositoryName