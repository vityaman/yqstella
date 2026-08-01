{-# LANGUAGE TupleSections #-}

module Type.Exception (annotateExceptionExprType) where

import Annotation (Annotated (annotation))
import Diagnostic.Code (Code (AMBIGUOUS_PANIC_TYPE, AMBIGUOUS_THROW_TYPE, EXCEPTION_TYPE_NOT_DECLARED))
import Diagnostic.Core (Severity (..), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf)
import Control.Monad.RWS
import Type.Context (exceptionType)
import Control.Monad (when)

annotateExceptionExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateExceptionExprType Nothing (AST.Panic p) _ = do
  let message = "type inference for panic is not supported (use type ascriptions)"
  tell [diagnostic Error AMBIGUOUS_PANIC_TYPE (pointRange p) message]
  return (AST.Panic (p, Nothing))
annotateExceptionExprType t@(Just _) (AST.Panic p) _ = do
  return (AST.Panic (p, t))
annotateExceptionExprType Nothing (AST.Throw p expr) annotateType = do
  let message = "type inference for throw is not supported (use type ascriptions)"
  tell [diagnostic Error AMBIGUOUS_THROW_TYPE (pointRange p) message]
  expr' <- annotateType Nothing expr
  return (AST.Throw (p, Nothing) expr')
annotateExceptionExprType (Just t) (AST.Throw p expr) annotateType = do
  context <- get

  let exceptionT = exceptionType context
  when (null exceptionT) $ do
    let message = "expection type is not declared"
    tell [diagnostic Error EXCEPTION_TYPE_NOT_DECLARED (pointRange p) message]

  expr' <- annotateType exceptionT expr

  let t' = exceptionT >> typeOf expr' >> Just t
  return (AST.Throw (p, t') expr')
annotateExceptionExprType _ x@(AST.TryCatch {}) _ = do
  tell [notImplemented (annotation x) "TryCatch"]
  return $ fmap (,Nothing) x
annotateExceptionExprType t (AST.TryWith p try catch) annotateType = do
  try' <- annotateType t try
  let t' = typeOf try'
  catch' <- annotateType t' catch
  return $ AST.TryWith (p, t') try' catch'
annotateExceptionExprType _ _ _ = error "Unexpected non-exception expression"
