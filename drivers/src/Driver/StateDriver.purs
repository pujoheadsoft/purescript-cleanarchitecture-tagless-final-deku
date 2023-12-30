module Driver.StateDriver where

import Prelude

import Control.Monad.State (class MonadState, modify_)
import Data.Either (Either(..))
import Effect (Effect)
import Presenter.Port (GitHubRepositoryPresenterPortFunction)
import State.SearchGitHubRepositoryState (GitHubRepositories, SearchGitHubRepositoryState, ErrorMessage)

presenterPortFunction :: forall m. MonadState SearchGitHubRepositoryState m => GitHubRepositoryPresenterPortFunction m
presenterPortFunction = {
  setRepositories: \r -> modify_ (_ { repositories = Right r }),
  setLoading: \loading -> modify_ (_ { isLoading = loading }),
  setErrorMessage: \m -> modify_ (_ { repositories = Left m })
}

type StateFunctions = {
  setLoading :: Boolean -> Effect Unit,
  setRepositories :: GitHubRepositories -> Effect Unit,
  setErrorMessage :: ErrorMessage -> Effect Unit
}