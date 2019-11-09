module ForElmTest (fromResult, ResultForElmTest (..)) where

import qualified AST.Canonical as Can
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Name as Name
import qualified Data.Text as T
import Data.Text (Text)
import qualified Elm.Details
import Elm.Interface
import qualified Elm.ModuleName as ModuleName
import qualified Elm.Package as Pkg
import GHC.Generics (Generic)
import qualified Info
import qualified Result

data ResultForElmTest
  = ResultForElmTest
      { outline :: Elm.Details.ValidOutline,
        testModules :: [TestModule]
      }
  deriving (Generic)

instance Aeson.ToJSON ResultForElmTest

data TestModule
  = TestModule
      { moduleName :: Text,
        path :: Text,
        tests :: [Text]
      }
  deriving (Generic)

instance Aeson.ToJSON TestModule

fromResult :: Result.Result -> ResultForElmTest
fromResult Result.Result {Result.details, Result.internals} =
  ResultForElmTest
    { testModules = mapMaybe toTestModules internals,
      outline = case details of
        Info.Details Elm.Details.Details {Elm.Details._outline} -> _outline
    }

toTestModules :: Info.Internal -> Maybe TestModule
toTestModules Info.Internal {Info.moduleName, Info.modulePath, Info.interface} =
  case onlyTests (_values interface) of
    [] -> Nothing
    tests -> Just TestModule {moduleName, path = T.pack modulePath, tests}

onlyTests :: Map.Map Name.Name Can.Annotation -> [Text]
onlyTests = mapMaybe isTest . Map.toList

isTest :: (Name.Name, Can.Annotation) -> Maybe Text
isTest (value, annotation) =
  case annotation of
    Can.Forall
      _
      ( Can.TAlias
          ModuleName.Canonical
            { ModuleName._package,
              ModuleName._module
            }
          name
          _
          _
        ) ->
        if Name.toChars name == "Test"
          && Pkg.toChars _package == "elm-explorations/test"
          && Name.toChars _module == "Test"
          then Just (T.pack $ Name.toChars value)
          else Nothing
    _ -> Nothing
