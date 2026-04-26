module Type.Context
  ( Context,
    empty,
    withTyped,
    withTypeAliased,
    typeOf,
    typeWithAlias,
    unknownName,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagnostic.Code (Code (UNDEFINED_VARIABLE))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange)
import Type.Core (Type)

newtype Binding = Binding Type
  deriving (Show)

data Context = Context (Map String Binding) (Map String Type)
  deriving (Show)

empty :: Context
empty = Context Map.empty Map.empty

withTyped :: String -> Type -> Context -> Context
withTyped key t (Context bindings typeAliases) =
  Context (Map.insert key (Binding t) bindings) typeAliases

withTypeAliased :: String -> Type -> Context -> Context
withTypeAliased key t (Context bindings typeAliases) =
  Context bindings (Map.insert key t typeAliases)

typeOf :: String -> Context -> Maybe Type
typeOf key (Context bindings _) = (\(Binding x) -> x) <$> Map.lookup key bindings

typeWithAlias :: String -> Context -> Maybe Type
typeWithAlias key (Context _ typeAliases) = Map.lookup key typeAliases

unknownName :: Position -> String -> Diagnostic
unknownName position name =
  let message = "undefined variable " ++ name
   in diagnostic Error UNDEFINED_VARIABLE (pointRange position) message
