{-# LANGUAGE TupleSections #-}

module Type.Exception (annotateExceptionExprType) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (tell)
import Diagnostic.Code (Code (AMBIGUOUS_PANIC_TYPE))
import Diagnostic.Core (Severity (..), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
import Type.Env (TypeAnnotationEnv, TypeAnnotator)

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
annotateExceptionExprType _ x@(AST.Throw {}) _ = do
  tell [notImplemented (annotation x) "Throw"]
  return $ fmap (,Nothing) x
annotateExceptionExprType _ x@(AST.TryCatch {}) _ = do
  tell [notImplemented (annotation x) "TryCatch"]
  return $ fmap (,Nothing) x
annotateExceptionExprType _ x@(AST.TryWith {}) _ = do
  tell [notImplemented (annotation x) "TryWith"]
  return $ fmap (,Nothing) x
annotateExceptionExprType _ _ _ = error "Unexpected non-exception expression"
