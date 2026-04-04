module Type.PatternMatching (bindings, isExhaustive) where

import Annotation (Annotated (annotation))
import Data.Map (Map)
import qualified Data.Map as Map
import Diagnostic.Code (Code (UNEXPECTED_PATTERN_FOR_TYPE))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Cardinality (Cardinality, cardinality, (+?+))
import Type.Core (Type (Type))
import qualified Type.Core as Type

type Exhaustiveness = Cardinality

bindings :: AST.Pattern' Position -> Type -> Either Diagnostic (Map String Type)
bindings (AST.PatternCastAs p _ _) _ =
  Left $ notImplemented p "PatternCastAs Matching"
bindings (AST.PatternAsc p _ _) _ =
  Left $ notImplemented p "PatternAsc Matching"
bindings (AST.PatternVariant p _ _) _ =
  Left $ notImplemented p "PatternVariant Matching"
bindings (AST.PatternInl _ pattern') (Type (AST.TypeSum _ inl _)) =
  bindings pattern' (Type.fromAST inl)
bindings (AST.PatternInr _ pattern') (Type (AST.TypeSum _ _ inr)) =
  bindings pattern' (Type.fromAST inr)
bindings (AST.PatternTuple p _) _ =
  Left $ notImplemented p "PatternTuple Matching"
bindings (AST.PatternRecord p _) _ =
  Left $ notImplemented p "PatternRecord Matching"
bindings (AST.PatternList p _) _ =
  Left $ notImplemented p "PatternList Matching"
bindings (AST.PatternCons p _ _) _ =
  Left $ notImplemented p "PatternCons Matching"
bindings (AST.PatternFalse p) _ =
  Left $ notImplemented p "PatternFalse Matching"
bindings (AST.PatternTrue p) _ =
  Left $ notImplemented p "PatternTrue Matching"
bindings (AST.PatternUnit p) _ =
  Left $ notImplemented p "PatternUnit Matching"
bindings (AST.PatternInt p _) _ =
  Left $ notImplemented p "PatternInt Matching"
bindings (AST.PatternSucc p _) _ =
  Left $ notImplemented p "PatternSucc Matching"
bindings (AST.PatternVar _ (AST.StellaIdent name)) t =
  Right $ Map.singleton name t
bindings p t =
  let position = pointRange $ annotation p
      message = "unexpected pattern for type " ++ show t
   in Left $ diagnostic Error UNEXPECTED_PATTERN_FOR_TYPE position message

isExhaustive :: [AST.Pattern' Position] -> Type -> Bool
isExhaustive ps (Type t) =
  isExhaustive' $ foldr exhaustiveness' (fmap (const $ Just 0) t) ps

-- TODO(vityaman): #structural-patterns
isExhaustive' :: AST.Type' Exhaustiveness -> Bool
isExhaustive' (AST.TypeSum _ inl inr) =
  isExhaustive' inl && isExhaustive' inr
isExhaustive' t =
  annotation t == cardinality (Type.fromAST t)

exhaustiveness' :: AST.Pattern' Position -> AST.Type' Exhaustiveness -> AST.Type' Exhaustiveness
exhaustiveness' (AST.PatternInl _ pattern') (AST.TypeSum _ inl inr) =
  let inl' = exhaustiveness' pattern' inl
   in AST.TypeSum (annotation inl' +?+ annotation inr) inl' inr
exhaustiveness' (AST.PatternInr _ pattern') (AST.TypeSum _ inl inr) =
  let inr' = exhaustiveness' pattern' inr
   in AST.TypeSum (annotation inl +?+ annotation inr') inl inr'
exhaustiveness' (AST.PatternVar _ _) t =
  fmap (const $ cardinality (Type.fromAST t)) t
exhaustiveness' _ t =
  fmap (const $ Just 0) t
