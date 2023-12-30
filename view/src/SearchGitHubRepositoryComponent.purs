module SearchGitHubRepositoryComponent where

import Prelude

import CSS (px, width)
import CSS.Overflow (hidden, overflow)
import CSS.Text.Overflow (ellipsis, textOverflow)
import CSS.Text.Whitespace (textWhitespace, whitespaceNoWrap)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.CSS as CSS
import Deku.Core (Nut, fixed)
import Deku.DOM as D
import Deku.DOM.Attributes as DA
import Deku.DOM.Listeners as DL
import Deku.Do as Deku
import Deku.Hooks (useRef, useState', (<#~>))
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

component_ = Proxy :: Proxy """
<div>
  <h1>Search GitHub Repository</h1>
  <label>
    <div>Enter repository name:</div>
    ~formMatter~
  </label>
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
    formMatter: Deku.do
      setName /\ name <- useState'
      ref <- useRef mempty name
      fixed
        [ D.input
            [ DA.xtypeText
            , DA.value name
            , DL.valueOn_ DL.change setName ]
            []
        , D.button
            [ DL.click_ \_ -> ref >>= searchRepositoryByName >>> launchAff_
            , DA.disabled $ isLoading <#> show ]
            [ D.text_ "Search" ]
        ]
      
    , result: repositories <#~> renderRepositories
  }
  where
  renderRepositories = case _ of
    Left err ->
      D.div_ [ D.text_ $ "Failed loading repositories: " <> err ]
    Right r ->
      D.div_ (renderRepository <$> r)

  renderRepository repository =
    D.div
      [ DA.style_ "display: flex; column-gap: 8px;" ] 
      [ D.div [ DA.style_ styleOwner ] [D.text_ repository.owner]
      , D.div [ DA.style_ styleUrl ] [D.a [DA.href_ repository.url ] [ D.text_ repository.name ]]
      , D.div [ DA.style_ styleUpdateDate ] [D.text_ repository.updateDate]
      ]

  styleOwner = CSS.render do
    width $ px 150.0
    overflow hidden
    textWhitespace whitespaceNoWrap
    textOverflow ellipsis

  styleUrl = CSS.render do
    width $ px 350.0
    overflow hidden
    textWhitespace whitespaceNoWrap
    textOverflow ellipsis

  styleUpdateDate = CSS.render do 
    width $ px 100.0
