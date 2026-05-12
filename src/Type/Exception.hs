{-# LANGUAGE TupleSections #-}

module Type.Exception (annotateExceptionExprType) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (tell)
import Diagnostic.Core (notImplemented)
import Diagnostic.Position (Position)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
import Type.Env (TypeAnnotationEnv, TypeAnnotator)

annotateExceptionExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateExceptionExprType _ x@(AST.Panic {}) _ = do
  tell [notImplemented (annotation x) "Panic"]
  return $ fmap (,Nothing) x
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
