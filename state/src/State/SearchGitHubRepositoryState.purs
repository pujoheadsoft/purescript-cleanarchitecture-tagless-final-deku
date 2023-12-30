module State.SearchGitHubRepositoryState where

type GitHubRepositories
  = Array GitHubRepository

type GitHubRepository
  = { name :: String
    , url :: String
    , owner :: String
    , updateDate :: String
    }

type ErrorMessage = String