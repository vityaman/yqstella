module Type.Sum (annotateSumExprType) where

import Control.Monad (unless)
import Control.Monad.State (gets)
import Diagnostic.Code (Code (AMBIGUOUS_SUM_TYPE, UNEXPECTED_INJECTION))
import Diagnostic.Core (Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Constraint as Constraint
import qualified Type.Context as Context
import Type.Core (Type (..))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, freshTypeVar, isAvailable, tellC, tellD, typeOf)
import qualified Type.Unification as Unification

annotateSumExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateSumExprType Nothing (AST.Inl p expr) annotateType = do
  expr' <- annotateType Nothing expr -- TODO: make a function for each diagnostic
  isBottom <- isAvailable Extension.AmbiguousTypeAsBottom
  isTypeReconstruction <- isAvailable Extension.TypeReconstruction
  unless (isBottom || isTypeReconstruction) $
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tellD [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]

  let inlT = typeOf expr'
  inrT <-
    if isTypeReconstruction
      then Just <$> freshTypeVar
      else pure $ if isBottom then Just $ Type.fromAST' AST.TypeBottom else Nothing
  let t' = (\(Type x) (Type y) -> Type (AST.TypeSum () x y)) <$> inlT <*> inrT
  return (AST.Inl (p, t') expr')
annotateSumExprType (Just (Type (AST.TypeTop ()))) (AST.Inl p expr) annotateType =
  annotateSumExprType Nothing (AST.Inl p expr) annotateType
annotateSumExprType (Just variable@(Type (AST.TypeVar () _))) (AST.Inl p expr) annotateType = do
  metaVariables <- gets Context.metaVars
  if Unification.isMetaVar metaVariables variable
    then do
      expr' <- annotateType Nothing expr
      (Type right) <- freshTypeVar
      let inferred = (\(Type left) -> Type (AST.TypeSum () left right)) <$> typeOf expr'
      maybe (pure ()) (tellC . pure . Constraint.Eq p variable) inferred
      return (AST.Inl (p, inferred) expr')
    else annotateSumExprTypeUnexpected (Just variable) (AST.Inl p expr) annotateType
annotateSumExprType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inl p expr) annotateType = do
  expr' <- annotateType (Just (Type inl)) expr
  let t' = (\(Type x) -> Type (AST.TypeSum () x inr)) <$> typeOf expr'
  return (AST.Inl (p, t') expr')
annotateSumExprType t@(Just _) expression@(AST.Inl {}) annotateType =
  annotateSumExprTypeChecked t expression annotateType
annotateSumExprType Nothing (AST.Inr p expr) annotateType = do
  expr' <- annotateType Nothing expr
  isBottom <- isAvailable Extension.AmbiguousTypeAsBottom
  isTypeReconstruction <- isAvailable Extension.TypeReconstruction
  unless (isBottom || isTypeReconstruction) $
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tellD [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]

  inlT <-
    if isTypeReconstruction
      then Just <$> freshTypeVar
      else pure $ if isBottom then Just $ Type.fromAST' AST.TypeBottom else Nothing
  let inrT = typeOf expr'
      t' = (\(Type x) (Type y) -> Type (AST.TypeSum () x y)) <$> inlT <*> inrT
  return (AST.Inr (p, t') expr')
annotateSumExprType (Just (Type (AST.TypeTop ()))) (AST.Inr p expr) annotateType =
  annotateSumExprType Nothing (AST.Inr p expr) annotateType
annotateSumExprType (Just variable@(Type (AST.TypeVar () _))) (AST.Inr p expr) annotateType = do
  metaVariables <- gets Context.metaVars
  if Unification.isMetaVar metaVariables variable
    then do
      expr' <- annotateType Nothing expr
      (Type left) <- freshTypeVar
      let inferred = (\(Type right) -> Type (AST.TypeSum () left right)) <$> typeOf expr'
      maybe (pure ()) (tellC . pure . Constraint.Eq p variable) inferred
      return (AST.Inr (p, inferred) expr')
    else annotateSumExprTypeUnexpected (Just variable) (AST.Inr p expr) annotateType
annotateSumExprType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inr p expr) annotateType = do
  expr' <- annotateType (Just (Type inr)) expr
  let t' = (\(Type x) -> Type (AST.TypeSum () inl x)) <$> typeOf expr'
  return (AST.Inr (p, t') expr')
annotateSumExprType t@(Just _) expression@(AST.Inr {}) annotateType =
  annotateSumExprTypeChecked t expression annotateType
annotateSumExprType _ _ _ = error "Unexpected non-sum expression"

annotateSumExprTypeUnexpected ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateSumExprTypeUnexpected (Just t) (AST.Inl p expr) annotateType = do
  expr' <- annotateType Nothing expr
  let expr't = maybe "?" show $ typeOf expr'
      message = "expected " ++ show t ++ ", but got inl(" ++ expr't ++ ")"
   in tellD [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
  return (AST.Inl (p, Nothing) expr')
annotateSumExprTypeUnexpected (Just t) (AST.Inr p expr) annotateType = do
  expr' <- annotateType Nothing expr
  let expr't = maybe "?" show $ typeOf expr'
      message = "expected " ++ show t ++ ", but got inr(" ++ expr't ++ ")"
   in tellD [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
  return (AST.Inr (p, Nothing) expr')
annotateSumExprTypeUnexpected _ _ _ = error "Unexpected sum expression"

annotateSumExprTypeChecked ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateSumExprTypeChecked expected expression annotateType = do
  isTypeReconstruction <- isAvailable Extension.TypeReconstruction
  if isTypeReconstruction
    then do
      expression' <- annotateSumExprType Nothing expression annotateType
      case (expected, typeOf expression') of
        (Just expected', Just actual) -> tellC [Constraint.Eq (position expression) actual expected']
        _ -> pure ()
      return expression'
    else annotateSumExprTypeUnexpected expected expression annotateType
  where
    position (AST.Inl p _) = p
    position (AST.Inr p _) = p
    position _ = error "Unexpected sum expression"
