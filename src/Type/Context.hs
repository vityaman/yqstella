module Type.Context
  ( Context,
    empty,
    withTyped,
    withTypeAliased,
    withExceptionType,
    typeOf,
    typeWithAlias,
    exceptionType,
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

data Context = Context
  { contextBindings :: Map String Binding,
    contextTypeAliases :: Map String Type,
    contextExceptionType :: Maybe Type
  }
  deriving (Show)

empty :: Context
empty = Context Map.empty Map.empty Nothing

withTyped :: String -> Type -> Context -> Context
withTyped key t (Context bindings typeAliases exceptionT) =
  Context (Map.insert key (Binding t) bindings) typeAliases exceptionT

withTypeAliased :: String -> Type -> Context -> Context
withTypeAliased key t (Context bindings typeAliases exceptionT) =
  Context bindings (Map.insert key t typeAliases) exceptionT

withExceptionType :: Type -> Context -> Context
withExceptionType t (Context bindings typeAliases _) =
  Context bindings typeAliases (Just t)

typeOf :: String -> Context -> Maybe Type
typeOf key ctx = (\(Binding x) -> x) <$> Map.lookup key (contextBindings ctx)

typeWithAlias :: String -> Context -> Maybe Type
typeWithAlias key ctx = Map.lookup key (contextTypeAliases ctx)

exceptionType :: Context -> Maybe Type
exceptionType = contextExceptionType

unknownName :: Position -> String -> Diagnostic
unknownName position name =
  let message = "undefined variable " ++ name
   in diagnostic Error UNDEFINED_VARIABLE (pointRange position) message
