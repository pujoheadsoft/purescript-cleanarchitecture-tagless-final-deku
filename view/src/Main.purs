module Main where

import Prelude

import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Domain.GitHubRepository (GitHubRepositoryName(..))
import Driver.GitHubApiDriver (gitHubRepositoryGatewayPortFunction)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Gateway.GitHubRepositoryGateway (gitHubRepositoryPortFunction)
import Presenter.GitHubRepositoryPresenter (gitHubRepositoryPresenterPortFunction)
import Presenter.Port (GitHubRepositoryPresenterPortFunction)
import Record.Builder (build, merge)
import SearchGitHubRepositoryComponent (component)
import UseCase.SearchGitHubRepositoryUseCase (execute)


main :: Effect Unit
main = runInBody Deku.do
  setRepositories /\ repositrories <- useState'
  setLoading /\ isLoading <- useState'
  let
    presenterPortFunction :: forall m. MonadEffect m => GitHubRepositoryPresenterPortFunction m
    presenterPortFunction = {
      setRepositories: \r -> liftEffect $ setRepositories (Right r),
      setLoading: \loading -> liftEffect $ setLoading loading,
      setErrorMessage: \m -> liftEffect $ setRepositories (Left m)
    }
    searchRepositoryByName
      :: forall m
        . Monad m
      => MonadAff m
      => MonadEffect m
      => String
      -> m Unit
    searchRepositoryByName name = do
      let
        gf = gitHubRepositoryPortFunction gitHubRepositoryGatewayPortFunction
        pf = gitHubRepositoryPresenterPortFunction presenterPortFunction 
        functions = build (merge (gf)) pf
      runReaderT (execute (GitHubRepositoryName name)) functions
  (component repositrories isLoading (\name -> launchAff_ $ searchRepositoryByName name))