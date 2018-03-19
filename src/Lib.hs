module Lib
  ( run
  ) where

import Args (Args(..))
import qualified Args
import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson as Aeson
import qualified Data.Binary as B
import qualified Data.Text as T
import Elm.Interface (Interface)
import qualified Elmi
import GHC.Generics (Generic)
import Subset (Subset(..))

data Info = Info
  { moduleName :: T.Text
  , interface :: Interface
  } deriving (Generic)

instance Aeson.ToJSON Info

-- TODO error handling, change to ExceptT
run :: IO ()
run = do
  Args {infoFor} <- Args.parse
  modulePaths <-
    case infoFor of
      All -> Elmi.all
      Subset modulePaths -> return $ Elmi.fromModulePath <$> modulePaths
  result <- Async.mapConcurrently infoForModule modulePaths
  printJSON result

infoForModule :: FilePath -> IO Info
infoForModule modulePath = do
  interface <- B.decodeFile modulePath
  return Info {moduleName = Elmi.toModuleName modulePath, interface = interface}

printJSON :: [Info] -> IO ()
printJSON = print . Aeson.encode
