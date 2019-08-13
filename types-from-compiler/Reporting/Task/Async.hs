module Reporting.Task.Async
  ( mapConcurrently
  ) where

import qualified Reporting.Task as Task
import Reporting.Task (Task)
import qualified UnliftIO

mapConcurrently ::
     (Traversable t, UnliftIO.Exception err)
  => (a -> Task err b)
  -> t a
  -> Task err (t b)
mapConcurrently f =
  Task.eio id .
  UnliftIO.try . UnliftIO.mapConcurrently (UnliftIO.fromEitherM . Task.run . f)
