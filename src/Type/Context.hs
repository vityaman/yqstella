module Type.Context (Context, empty, withTyped, typeOf, unknownName) where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagnostic.Code (Code (UNDEFINED_VARIABLE))
import Diagnostic.Core (Diagnostic (Diagnostic), Severity (Error))
import Diagnostic.Position (Position, pointRange)
import Type.Core (Type)

newtype Binding = Binding Type

newtype Context = Context (Map String Binding)

empty :: Context
empty = Context Map.empty

withTyped :: String -> Type -> Context -> Context
withTyped key t (Context bindings) = Context $ Map.insert key (Binding t) bindings

typeOf :: String -> Context -> Maybe Type
typeOf key (Context bindings) = (\(Binding x) -> x) <$> Map.lookup key bindings

unknownName :: Position -> String -> Diagnostic
unknownName position name =
  let message = "undefined variable " ++ name
   in Diagnostic Error UNDEFINED_VARIABLE (pointRange position) message
