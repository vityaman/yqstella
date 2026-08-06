module Type.Subtyping (liftSubType, liftSubType') where

import Control.Monad.Writer (tell)
import Data.Foldable (find)
import Diagnostic.Code (Code (UNEXPECTED_SUBTYPE, UNEXPECTED_TYPE_FOR_EXPRESSION))
import Diagnostic.Core (Diagnostic (range), Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
import Type.Env
import Type.Expectation (mismatch)

liftSubType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv Type
liftSubType p lifting = liftSubType' p (Type $ lifting ())

liftSubType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv Type
liftSubType' p lifting (Just checked) =
  case lifting `subsumes` checked of
    Right () ->
      return lifting
    Left d -> do
      tell [d {range = pointRange p}]
      return lifting
liftSubType' _ lifting Nothing =
  return lifting

subsumes :: Type -> Type -> Either Diagnostic ()
subsumes lhs rhs | lhs == rhs = Right ()
subsumes (Type (AST.TypeRecord () lhs)) (Type (AST.TypeRecord () rhs)) =
  mapM_ (`subsumesF` rhs) lhs
  where
    nameOf' (AST.ARecordFieldType () (AST.StellaIdent x) _) = x
    typeOf' (AST.ARecordFieldType () _ t) = t

    subsumesF :: AST.RecordFieldType' () -> [AST.RecordFieldType' ()] -> Either Diagnostic ()
    subsumesF (AST.ARecordFieldType () (AST.StellaIdent rhsName) rhsT) lhs' =
      case find (\lhsF -> rhsName == nameOf' lhsF) lhs' of
        Nothing ->
          let message = "(subsumes) missing record field: " ++ rhsName
           in Left $ diagnostic Error UNEXPECTED_SUBTYPE (pointRange unknown) message
        Just lhsF ->
          let lhsT = typeOf' lhsF
           in subsumes (Type lhsT) (Type rhsT)
subsumes lhs rhs =
  Left $ mismatch UNEXPECTED_TYPE_FOR_EXPRESSION unknown lhs rhs
