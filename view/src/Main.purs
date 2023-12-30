module Main where

import Prelude

import Deku.Toplevel (runInBody)
import Effect (Effect)
import SearchGitHubRepositoryComponent (component)

main :: Effect Unit
main = runInBody component
