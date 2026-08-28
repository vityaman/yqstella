module Type.Context
  ( Context,
    empty,
    withName,
    withTyped,
    withTypeAliased,
    withTypeVariables,
    bindTypeVariables,
    withFreshTypeVar,
    metaVars,
    restoreInferenceState,
    withExceptionType,
    withExceptionVariant,
    typeOf,
    typeWithAlias,
    resolveTypeVariable,
    exceptionType,
    isAvailable,
    unknownName,
  )
where

import Data.Foldable (find)
import Data.List (intercalate)
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
  { contextName :: [String],
    contextBindings :: Map String Binding,
    contextTypeAliases :: Map String Type,
    contextTypeVariables :: Map String AST.StellaIdent,
    contextExceptionType :: Maybe Type,
    contextExceptionTypeMode :: ExceptionTypeMode,
    contextExtensions :: Extensions,
    contextPrevId :: Int,
    contextMetaVars :: Set.Set String
  }
  deriving (Show)

empty :: Extensions -> Context
empty extensions =
  Context
    { contextName = [],
      contextBindings = Map.empty,
      contextTypeAliases = Map.empty,
      contextTypeVariables = Map.empty,
      contextExceptionType = Nothing,
      contextExceptionTypeMode = Unknown,
      contextExtensions = extensions,
      contextPrevId = 0,
      contextMetaVars = Set.empty
    }

withName :: String -> Context -> Context
withName name c = c {contextName = name : contextName c}

withTyped :: String -> Type -> Context -> Context
withTyped key t c@(Context {contextBindings = bindings}) =
  c {contextBindings = Map.insert key (Binding t) bindings}

withTypeAliased :: String -> Type -> Context -> Context
withTypeAliased key t c@(Context {contextTypeAliases = typeAliases}) =
  c {contextTypeAliases = Map.insert key t typeAliases}

withTypeVariables :: [AST.StellaIdent] -> Context -> Context
withTypeVariables names = snd . bindTypeVariables names

bindTypeVariables :: [AST.StellaIdent] -> Context -> ([AST.StellaIdent], Context)
bindTypeVariables parameters context =
  let (resolved, variables) = foldl bind ([], contextTypeVariables context) parameters
   in (resolved, context {contextTypeVariables = variables})
  where
    bind (resolved, variables) parameter =
      let source = name parameter
          used = Set.fromList (Map.keys variables ++ fmap name (Map.elems variables))
          resolvedName =
            if source `Set.member` used
              then freshName used source
              else source
          resolvedParameter = AST.StellaIdent resolvedName
       in (resolved ++ [resolvedParameter], Map.insert source resolvedParameter variables)
    name (AST.StellaIdent value) = value

freshName :: Set.Set String -> String -> String
freshName used base = go (1 :: Int)
  where
    go n =
      let candidate = base ++ "_" ++ show n
       in if candidate `Set.member` used then go (n + 1) else candidate

withFreshTypeVar :: Context -> (Type, Context)
withFreshTypeVar c =
  let nextId = contextPrevId c + 1
      name = "TypeVar(" ++ intercalate " |> " (reverse $ contextName c) ++ " |> " ++ show nextId ++ ")"
   in ( Type (AST.TypeVar () (AST.StellaIdent name)),
        c
          { contextPrevId = nextId,
            contextMetaVars = Set.insert name (contextMetaVars c)
          }
      )

metaVars :: Context -> Set.Set String
metaVars = contextMetaVars

restoreInferenceState :: Context -> Context -> Context
restoreInferenceState inferred lexical =
  lexical
    { contextPrevId = max (contextPrevId inferred) (contextPrevId lexical),
      contextMetaVars = contextMetaVars inferred <> contextMetaVars lexical
    }

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

resolveTypeVariable :: String -> Context -> Maybe AST.StellaIdent
resolveTypeVariable key ctx = Map.lookup key (contextTypeVariables ctx)

exceptionType :: Context -> Maybe Type
exceptionType = contextExceptionType

isAvailable :: Context -> Extension -> Bool
isAvailable Context {contextExtensions = es} e = Set.member e es

unknownName :: Position -> String -> Diagnostic
unknownName position name =
  let message = "undefined variable " ++ name
   in diagnostic Error UNDEFINED_VARIABLE (pointRange position) message
