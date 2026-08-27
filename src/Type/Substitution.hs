module Type.Substitution
  ( Substitution,
    empty,
    singleton,
    insert,
    apply,
    substitute,
    applyConstraint,
    applyProgram,
  )
where

import Control.Monad (when)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Diagnostic.Code (Code (DEBUG))
import Diagnostic.Core (Severity (Info), diagnostic)
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
apply (Substitution substitutions) = applyWith substitutions

substitute :: [(AST.StellaIdent, Type)] -> Type -> Type
substitute bindings = apply (Substitution substitutions)
  where
    substitutions = Map.fromList [(identName parameter, type_) | (parameter, type_) <- bindings]

applyWith :: Map String Type -> Type -> Type
applyWith substitutions (Type type_) = Type (visit substitutions type_)
  where
    visit current type' =
      case type' of
        AST.TypeAuto () -> AST.TypeAuto ()
        AST.TypeFun () args ret -> AST.TypeFun () (fmap (visit current) args) (visit current ret)
        AST.TypeForAll () parameters body ->
          let (parameters', body', scoped) = enterScope current parameters body
           in AST.TypeForAll () parameters' (visit scoped body')
        AST.TypeRec () parameter body ->
          let (parameter', body', scoped) = enterBinder current parameter body
           in AST.TypeRec () parameter' (visit scoped body')
        AST.TypeSum () lhs rhs -> AST.TypeSum () (visit current lhs) (visit current rhs)
        AST.TypeTuple () types -> AST.TypeTuple () (fmap (visit current) types)
        AST.TypeRecord () fields -> AST.TypeRecord () (fmap (recordField current) fields)
        AST.TypeVariant () fields -> AST.TypeVariant () (fmap (variantField current) fields)
        AST.TypeList () item -> AST.TypeList () (visit current item)
        AST.TypeBool () -> AST.TypeBool ()
        AST.TypeNat () -> AST.TypeNat ()
        AST.TypeUnit () -> AST.TypeUnit ()
        AST.TypeTop () -> AST.TypeTop ()
        AST.TypeBottom () -> AST.TypeBottom ()
        AST.TypeRef () item -> AST.TypeRef () (visit current item)
        AST.TypeVar () ident -> maybe (AST.TypeVar () ident) Type.toAST (Map.lookup (identName ident) current)

    enterScope current parameters body =
      let scoped = foldr (Map.delete . identName) current parameters
          captured = Set.unions (fv <$> Map.elems scoped)
          used = typeNames body <> captured <> Map.keysSet scoped <> Set.fromList (fmap identName parameters)
          (parameters', body') = renameCaptured captured used parameters body
       in (parameters', body', scoped)

    enterBinder current parameter body =
      let scoped = Map.delete (identName parameter) current
          captured = Set.unions (fv <$> Map.elems scoped)
          used = typeNames body <> captured <> Map.keysSet scoped <> Set.singleton (identName parameter)
       in if identName parameter `Set.member` captured
            then
              let parameter' = AST.StellaIdent (freshName used (identName parameter))
               in (parameter', renameBound parameter parameter' body, scoped)
            else (parameter, body, scoped)

    renameCaptured captured used parameters body =
      let (parameters', body', _) = foldl renameOne (parameters, body, used) parameters
       in (parameters', body')
      where
        renameOne (parameters', body', used') parameter
          | identName parameter `Set.member` captured =
              let parameter' = AST.StellaIdent (freshName used' (identName parameter))
               in (replace parameter parameter' parameters', renameBound parameter parameter' body', Set.insert (identName parameter') used')
          | otherwise = (parameters', body', used')

    replace old new = fmap (\value -> if value == old then new else value)

    renameBound old new = rename
      where
        rename type' = case type' of
          AST.TypeAuto () -> type'
          AST.TypeFun () args ret -> AST.TypeFun () (fmap rename args) (rename ret)
          AST.TypeForAll () parameters body
            | old `elem` parameters -> AST.TypeForAll () parameters body
            | otherwise -> AST.TypeForAll () parameters (rename body)
          AST.TypeRec () parameter body
            | old == parameter -> AST.TypeRec () parameter body
            | otherwise -> AST.TypeRec () parameter (rename body)
          AST.TypeSum () lhs rhs -> AST.TypeSum () (rename lhs) (rename rhs)
          AST.TypeTuple () types -> AST.TypeTuple () (fmap rename types)
          AST.TypeRecord () fields -> AST.TypeRecord () (fmap (mapRecordField rename) fields)
          AST.TypeVariant () fields -> AST.TypeVariant () (fmap (mapVariantField rename) fields)
          AST.TypeList () item -> AST.TypeList () (rename item)
          AST.TypeRef () item -> AST.TypeRef () (rename item)
          AST.TypeVar () parameter | parameter == old -> AST.TypeVar () new
          _ -> type'

    recordField current (AST.ARecordFieldType () field fieldType) =
      AST.ARecordFieldType () field (visit current fieldType)

    variantField _ field@(AST.AVariantFieldType () _ (AST.NoTyping ())) = field
    variantField current (AST.AVariantFieldType () field (AST.SomeTyping () fieldType)) =
      AST.AVariantFieldType () field (AST.SomeTyping () (visit current fieldType))

    mapRecordField f (AST.ARecordFieldType () field fieldType) =
      AST.ARecordFieldType () field (f fieldType)

    mapVariantField f (AST.AVariantFieldType () field (AST.SomeTyping () fieldType)) =
      AST.AVariantFieldType () field (AST.SomeTyping () (f fieldType))
    mapVariantField _ field = field

    typeNames type' = case type' of
      AST.TypeAuto () -> mempty
      AST.TypeFun () args ret -> Set.unions (typeNames ret : fmap typeNames args)
      AST.TypeForAll () parameters body -> Set.fromList (fmap identName parameters) <> typeNames body
      AST.TypeRec () parameter body -> Set.insert (identName parameter) (typeNames body)
      AST.TypeSum () lhs rhs -> typeNames lhs <> typeNames rhs
      AST.TypeTuple () types -> Set.unions (fmap typeNames types)
      AST.TypeRecord () fields -> Set.unions [typeNames fieldType | AST.ARecordFieldType () _ fieldType <- fields]
      AST.TypeVariant () fields -> Set.unions (fmap variantNames fields)
      AST.TypeList () item -> typeNames item
      AST.TypeRef () item -> typeNames item
      AST.TypeVar () ident -> Set.singleton (identName ident)
      _ -> mempty

    variantNames (AST.AVariantFieldType () _ (AST.SomeTyping () fieldType)) = typeNames fieldType
    variantNames _ = mempty

freshName :: Set.Set String -> String -> String
freshName used base = findFresh (1 :: Int)
  where
    findFresh index =
      let candidate = base ++ "_" ++ show index
       in if candidate `Set.member` used then findFresh (index + 1) else candidate

identName :: AST.StellaIdent -> String
identName (AST.StellaIdent value) = value

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
