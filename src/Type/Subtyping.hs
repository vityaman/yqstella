module Type.Subtyping (liftSubType, liftSubType') where

import Control.Monad (when, zipWithM_)
import Control.Monad.Writer (tell)
import Data.Foldable (find)
import Diagnostic.Code (Code (INCORRECT_NUMBER_OF_ARGUMENTS, MISSING_RECORD_FIELDS, UNEXPECTED_SUBTYPE, UNEXPECTED_TYPE_FOR_NULLARY_LABEL))
import Diagnostic.Core as Diagnostic
import Diagnostic.Position (Position, pointRange, unknown)
import Syntax.PrettyPrint (displayAST)
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
subsumes _ (Type (AST.TypeTop ())) = Right ()
subsumes (Type (AST.TypeBottom ())) _ = Right ()
subsumes (Type (AST.TypeFun () lhsArgs lhsRet)) (Type (AST.TypeFun () rhsArgs rhsRet)) = do
  let lhsArgsLen = length lhsArgs
      rhsArgsLen = length rhsArgs
  when (lhsArgsLen /= rhsArgsLen) $
    let message' = "expected " ++ show lhsArgs ++ " arguments, got " ++ show rhsArgsLen
     in Left $ diagnostic Error INCORRECT_NUMBER_OF_ARGUMENTS (pointRange unknown) message'

  -- TODO(103): improve diagnostics message
  zipWithM_ subsumes (Type <$> rhsArgs) (Type <$> lhsArgs)
  subsumes (Type lhsRet) (Type rhsRet)
subsumes (Type (AST.TypeSum () lhsL lhsR)) (Type (AST.TypeSum () rhsL rhsR)) = do
  subsumes (Type lhsL) (Type rhsL)
  subsumes (Type lhsR) (Type rhsR)
subsumes (Type (AST.TypeTuple () lhs)) (Type (AST.TypeTuple () rhs)) =
  zipWithM_ subsumes (Type <$> lhs) (Type <$> rhs)
subsumes lhsT'@(Type (AST.TypeRecord () lhs)) rhsT'@(Type (AST.TypeRecord () rhs)) =
  mapM_ (`subsumesF` lhs) rhs
  where
    nameOf' (AST.ARecordFieldType () (AST.StellaIdent x) _) = x
    typeOf' (AST.ARecordFieldType () _ t) = t

    subsumesF :: AST.RecordFieldType' () -> [AST.RecordFieldType' ()] -> Either Diagnostic ()
    subsumesF (AST.ARecordFieldType () (AST.StellaIdent rhsName) rhsT) lhs' =
      case find (\lhsF -> rhsName == nameOf' lhsF) lhs' of
        Nothing ->
          let message' =
                ("(subsumes) missing record field: " ++ rhsName ++ ", ")
                  ++ ("checking " ++ show lhsT' ++ " <: " ++ show rhsT')
           in Left $ diagnostic Error MISSING_RECORD_FIELDS (pointRange unknown) message'
        Just lhsF ->
          let lhsT = typeOf' lhsF
           in Type lhsT `subsumes` Type rhsT
subsumes lhsT'@(Type (AST.TypeVariant () lhs)) rhsT'@(Type (AST.TypeVariant () rhs)) =
  mapM_ (`subsumesF` rhs) lhs
  where
    nameOf' (AST.AVariantFieldType () (AST.StellaIdent x) _) = x
    typingOf' (AST.AVariantFieldType () _ t) = t

    subsumesF :: AST.VariantFieldType' () -> [AST.VariantFieldType' ()] -> Either Diagnostic ()
    subsumesF (AST.AVariantFieldType () (AST.StellaIdent rhsName) rhsT) lhs' =
      case find (\lhsF -> rhsName == nameOf' lhsF) lhs' of
        Nothing ->
          let message' =
                ("(subsumes) unexpected variant field: " ++ rhsName ++ ", ")
                  ++ ("checking " ++ show lhsT' ++ " <: " ++ show rhsT')
           in Left $ diagnostic Error UNEXPECTED_SUBTYPE (pointRange unknown) message'
        Just lhsF ->
          let lhsT = typingOf' lhsF
           in case (lhsT, rhsT) of
                (AST.SomeTyping () lhsT'', AST.SomeTyping () rhsT'') ->
                  Type lhsT'' `subsumes` Type rhsT''
                (AST.NoTyping (), AST.NoTyping ()) ->
                  return ()
                (lhsT'', rhsT'') ->
                  let message' =
                        ("(subsumes) variant field " ++ show rhsName ++ " type mismatch: ")
                          ++ ("'" ++ displayAST lhsT'' ++ "' vs '" ++ displayAST rhsT'' ++ "'")
                   in Left $ diagnostic Error UNEXPECTED_TYPE_FOR_NULLARY_LABEL (pointRange unknown) message'
subsumes (Type (AST.TypeList () lhs)) (Type (AST.TypeList () rhs)) =
  subsumes (Type lhs) (Type rhs)
subsumes lhs rhs =
  let d = mismatch UNEXPECTED_SUBTYPE unknown lhs rhs
   in Left d {message = "(subsumes) " ++ message d}
