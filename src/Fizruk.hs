module Fizruk (prepared', prepared) where

import Diagnostic.Core (Diagnostic (..), Diagnostics, isFailure)
import Lib (Project (diagnostics, yql))

prepared' :: Diagnostics -> Diagnostics
prepared' = take 1 . filter (isFailure . severity)

prepared :: Project -> Project
prepared project =
  project
    { diagnostics = prepared' $ diagnostics project,
      yql = Nothing
    }
