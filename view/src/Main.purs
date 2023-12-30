module Main where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Deku.Do as Deku
import Deku.Hooks (useState, useState')
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Presenter.Port (GitHubRepositoryPresenterPortFunction)
import SearchGitHubRepositoryComponent (component)

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
  (component repositrories isLoading (\_ -> pure unit))