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
    isAvailable,
    unknownName,
  )
where

import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Diagnostic.Code (Code (CONFLICTING_EXCEPTION_DECLARATIONS, DUPLICATE_EXCEPTION_TYPE, DUPLICATE_EXCEPTION_VARIANT, ILLEGAL_LOCAL_OPEN_VARIANT_EXCEPTION, UNDEFINED_VARIABLE))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import Extension.Core (Extension, Extensions)
import Syntax.PrettyPrint
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (..))

newtype Binding = Binding Type
  deriving (Show)

data ExceptionTypeMode = Unknown | Atomic | OpenVariant
  deriving (Show)

data Context = Context
  { contextBindings :: Map String Binding,
    contextTypeAliases :: Map String Type,
    contextExceptionType :: Maybe Type,
    contextExceptionTypeMode :: ExceptionTypeMode,
    contextExtensions :: Extensions
  }
  deriving (Show)

empty :: Extensions -> Context
empty extensions =
  Context
    { contextBindings = Map.empty,
      contextTypeAliases = Map.empty,
      contextExceptionType = Nothing,
      contextExceptionTypeMode = Unknown,
      contextExtensions = extensions
    }

withTyped :: String -> Type -> Context -> Context
withTyped key t c@(Context {contextBindings = bindings}) =
  c {contextBindings = Map.insert key (Binding t) bindings}

withTypeAliased :: String -> Type -> Context -> Context
withTypeAliased key t c@(Context {contextTypeAliases = typeAliases}) =
  c {contextTypeAliases = Map.insert key t typeAliases}

withExceptionType :: Type -> Context -> Either Diagnostic Context
withExceptionType t c@Context {contextExceptionTypeMode = Unknown} =
  Right $ c {contextExceptionType = Just t, contextExceptionTypeMode = Atomic}
withExceptionType _ Context {contextExceptionTypeMode = Atomic} =
  let message = "exception type redefinition is not supported"
   in Left $ diagnostic Error DUPLICATE_EXCEPTION_TYPE (pointRange unknown) message
withExceptionType _ Context {contextExceptionTypeMode = OpenVariant} =
  let message = "cannot mix 'exception type' and 'exception variant' declarations"
   in Left $ diagnostic Error CONFLICTING_EXCEPTION_DECLARATIONS (pointRange unknown) message

withExceptionVariant :: String -> Type -> Context -> Either Diagnostic Context
withExceptionVariant _ _ Context {contextExceptionTypeMode = Atomic} =
  let message = "cannot mix 'exception type' and 'exception variant' declarations"
   in Left $ diagnostic Error CONFLICTING_EXCEPTION_DECLARATIONS (pointRange unknown) message
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

  case find (\(AST.AVariantFieldType _ (AST.StellaIdent n) _) -> n == name) alts of
    Just duplicate -> do
      let message = "exception variant conflicts with " ++ displayAST duplicate
      Left $ diagnostic Error DUPLICATE_EXCEPTION_VARIANT (pointRange unknown) message
    Nothing -> Right ()

  let newtypie = Type (AST.TypeVariant () $ alts ++ [newbie])
  return $ ctx {contextExceptionType = Just newtypie, contextExceptionTypeMode = OpenVariant}

typeOf :: String -> Context -> Maybe Type
typeOf key ctx = (\(Binding x) -> x) <$> Map.lookup key (contextBindings ctx)

typeWithAlias :: String -> Context -> Maybe Type
typeWithAlias key ctx = Map.lookup key (contextTypeAliases ctx)

exceptionType :: Context -> Maybe Type
exceptionType = contextExceptionType

isAvailable :: Context -> Extension -> Bool
isAvailable Context {contextExtensions = es} e = Set.member e es

unknownName :: Position -> String -> Diagnostic
unknownName position name =
  let message = "undefined variable " ++ name
   in diagnostic Error UNDEFINED_VARIABLE (pointRange position) message
