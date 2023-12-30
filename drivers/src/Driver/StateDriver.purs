module Driver.StateDriver where

import Prelude

import Effect (Effect)
import State.SearchGitHubRepositoryState (GitHubRepositories, ErrorMessage)

type StateFunctions = {
  setLoading :: Boolean -> Effect Unit,
  setRepositories :: GitHubRepositories -> Effect Unit,
  setErrorMessage :: ErrorMessage -> Effect Unit
}