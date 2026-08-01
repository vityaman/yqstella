module Type.Context
  ( Context,
    empty,
    withTyped,
    withTypeAliased,
    withExceptionType,
    withExceptionVariant,
    typeOf,
    typeWithAlias,
    exceptionType,
    unknownName,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Diagnostic.Code (Code (ILLEGAL_LOCAL_OPEN_VARIANT_EXCEPTION, UNDEFINED_VARIABLE))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (..))

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

withExceptionVariant :: String -> Type -> Context -> Either Diagnostic Context
withExceptionVariant name (Type t) ctx = do
  let newbie = AST.AVariantFieldType () (AST.StellaIdent name) (AST.SomeTyping () t)

  alts <- case exceptionType ctx of
    (Just (Type (AST.TypeVariant () alts'))) -> Right alts'
    (Just t') -> do
      let message =
            "expected variant exception type "
              ++ ("to add " ++ show newbie ++ ", ")
              ++ ("got " ++ show t')
      Left $ diagnostic Error ILLEGAL_LOCAL_OPEN_VARIANT_EXCEPTION (pointRange unknown) message
    Nothing -> Right []

  let newtypie = Type (AST.TypeVariant () $ alts ++ [newbie])
  return $ withExceptionType newtypie ctx

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
