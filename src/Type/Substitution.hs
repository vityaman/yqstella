module Type.Substitution
  ( Substitution,
    empty,
    singleton,
    insert,
    apply,
    applyConstraint,
    applyProgram,
    checkAmbiguity,
  )
where

import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Diagnostic.Code (Code (AMBIGUOUS_TYPE, DEBUG))
import Diagnostic.Core (Diagnostic, Diagnostics, Severity (Error, Fatal, Info), diagnostic)
import Diagnostic.Position (Position, pointRange)
import Extension.Core (Extension (DebugUnification))
import qualified SyntaxGen.AbsStella as AST
import Type.Constraint (Constraint (Eq))
import Type.Core (Type (Type), fv)
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, isAvailable, tellD)

newtype Substitution = Substitution (Map String Type)

empty :: Substitution
empty = Substitution Map.empty

singleton :: String -> Type -> Substitution
singleton name t = Substitution $ Map.singleton name t

insert :: String -> Type -> Substitution -> Substitution
insert name t substitution@(Substitution substitutions) =
  Substitution $ Map.insert name (apply substitution t) substitutions

apply :: Substitution -> Type -> Type
apply (Substitution substitutions) (Type t) = Type $ go substitutions t
  where
    go s type_ =
      case type_ of
        AST.TypeAuto () -> AST.TypeAuto ()
        AST.TypeFun () args ret -> AST.TypeFun () (fmap (go s) args) (go s ret)
        AST.TypeForAll () xs body -> AST.TypeForAll () xs (go (foldr remove s xs) body)
        AST.TypeRec () x body -> AST.TypeRec () x (go (remove x s) body)
        AST.TypeSum () lhs rhs -> AST.TypeSum () (go s lhs) (go s rhs)
        AST.TypeTuple () types -> AST.TypeTuple () (fmap (go s) types)
        AST.TypeRecord () fields -> AST.TypeRecord () (fmap (recordField s) fields)
        AST.TypeVariant () fields -> AST.TypeVariant () (fmap (variantField s) fields)
        AST.TypeList () item -> AST.TypeList () (go s item)
        AST.TypeBool () -> AST.TypeBool ()
        AST.TypeNat () -> AST.TypeNat ()
        AST.TypeUnit () -> AST.TypeUnit ()
        AST.TypeTop () -> AST.TypeTop ()
        AST.TypeBottom () -> AST.TypeBottom ()
        AST.TypeRef () item -> AST.TypeRef () (go s item)
        AST.TypeVar () ident@(AST.StellaIdent name) ->
          maybe (AST.TypeVar () ident) (Type.toAST . apply (Substitution s)) $ Map.lookup name s

    remove (AST.StellaIdent name) = Map.delete name

    recordField s (AST.ARecordFieldType () name t') =
      AST.ARecordFieldType () name (go s t')

    variantField _ field@(AST.AVariantFieldType () _ (AST.NoTyping ())) = field
    variantField s (AST.AVariantFieldType () name (AST.SomeTyping () t')) =
      AST.AVariantFieldType () name (AST.SomeTyping () (go s t'))

applyConstraint :: Substitution -> Constraint -> Constraint
applyConstraint substitution (Eq position lhs rhs) =
  Eq position (apply substitution lhs) (apply substitution rhs)

applyProgram :: Substitution -> AST.Program' (Position, Maybe Type) -> TypeAnnotationEnv (AST.Program' (Position, Maybe Type))
applyProgram substitution program = do
  isDebugUnification <- isAvailable DebugUnification
  traverse (applyAnnotation isDebugUnification) program
  where
    applyAnnotation _ annotation@(_, Nothing) = return annotation
    applyAnnotation isDebugUnification (position, Just t) = do
      let t' = apply substitution t
      when isDebugUnification $
        tellD [diagnostic Info DEBUG (pointRange position) (show t ++ " => " ++ show t')]
      return (position, Just t')

checkAmbiguity :: AST.Program' (Position, Maybe Type) -> Diagnostics
checkAmbiguity = foldMap checkAnnotation
  where
    checkAnnotation :: (Position, Maybe Type) -> Diagnostics
    checkAnnotation (_, Nothing) = mempty
    checkAnnotation (position, Just (Type (AST.TypeAuto ()))) =
      [diagnostic Fatal AMBIGUOUS_TYPE (pointRange position) "unexpected auto type after substitution"]
    checkAnnotation (position, Just t) =
      fmap (ambiguousTypeVariable position t) (Set.toList $ fv t)

    ambiguousTypeVariable :: Position -> Type -> String -> Diagnostic
    ambiguousTypeVariable position t variable =
      let message = "type " ++ show t ++ " has unresolved type variable " ++ variable
       in diagnostic Error AMBIGUOUS_TYPE (pointRange position) message
