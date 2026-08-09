module Type.Sum (annotateSumExprType) where

import Control.Monad (unless)
import Control.Monad.Writer (tell)
import Diagnostic.Code (Code (AMBIGUOUS_SUM_TYPE, UNEXPECTED_INJECTION))
import Diagnostic.Core (Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (..))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, isAvailable, typeOf)

annotateSumExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateSumExprType Nothing (AST.Inl p expr) annotateType = do
  expr' <- annotateType Nothing expr -- TODO: make a function for each diagnostic
  isBottom <- isAvailable Extension.AmbiguousTypeAsBottom
  unless isBottom $
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tell [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]

  let inlT = typeOf expr'
      inrT = if isBottom then Just $ Type.fromAST' AST.TypeBottom else Nothing
      t' = (\(Type x) (Type y) -> Type (AST.TypeSum () x y)) <$> inlT <*> inrT
  return (AST.Inl (p, t') expr')
annotateSumExprType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inl p expr) annotateType = do
  expr' <- annotateType (Just (Type inl)) expr
  let t' = (\(Type x) -> Type (AST.TypeSum () x inr)) <$> typeOf expr'
  return (AST.Inl (p, t') expr')
annotateSumExprType (Just t) (AST.Inl p expr) annotateType = do
  expr' <- annotateType Nothing expr
  let expr't = maybe "?" show $ typeOf expr'
      message = "expected " ++ show t ++ ", but got inl(" ++ expr't ++ ")"
   in tell [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
  return (AST.Inl (p, Nothing) expr')
annotateSumExprType Nothing (AST.Inr p expr) annotateType = do
  expr' <- annotateType Nothing expr
  isBottom <- isAvailable Extension.AmbiguousTypeAsBottom
  unless isBottom $
    let message = "type inference for sum types is not supported (use type ascriptions)"
     in tell [diagnostic Error AMBIGUOUS_SUM_TYPE (pointRange p) message]

  let inlT = if isBottom then Just $ Type.fromAST' AST.TypeBottom else Nothing
      inrT = typeOf expr'
      t' = (\(Type x) (Type y) -> Type (AST.TypeSum () x y)) <$> inlT <*> inrT
  return (AST.Inr (p, t') expr')
annotateSumExprType (Just (Type (AST.TypeSum _ inl inr))) (AST.Inr p expr) annotateType = do
  expr' <- annotateType (Just (Type inr)) expr
  let t' = (\(Type x) -> Type (AST.TypeSum () inl x)) <$> typeOf expr'
  return (AST.Inr (p, t') expr')
annotateSumExprType (Just t) (AST.Inr p expr) annotateType = do
  expr' <- annotateType Nothing expr
  let expr't = maybe "?" show $ typeOf expr'
      message = "expected " ++ show t ++ ", but got inr(" ++ expr't ++ ")"
   in tell [diagnostic Error UNEXPECTED_INJECTION (pointRange p) message]
  return (AST.Inr (p, Nothing) expr')
annotateSumExprType _ _ _ = error "Unexpected non-sum expression"
