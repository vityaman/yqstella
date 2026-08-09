{-# LANGUAGE TupleSections #-}

module Type.Exception (annotateExceptionExprType) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (unless, when)
import Control.Monad.RWS
import Diagnostic.Code (Code (AMBIGUOUS_PANIC_TYPE, AMBIGUOUS_THROW_TYPE, EXCEPTION_TYPE_NOT_DECLARED))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST
import Type.Context (exceptionType)
import Type.Core (Type)
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, isAvailable, typeOf)
import Type.Expectation (sanitizeT)
import Type.Match (annotateCaseType)

annotateExceptionExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateExceptionExprType Nothing (AST.Panic p) _ = do
  isBottom <- isAvailable Extension.AmbiguousTypeAsBottom
  unless isBottom $ do
    let message = "type inference for panic is not supported (use type ascriptions)"
    tell [diagnostic Error AMBIGUOUS_PANIC_TYPE (pointRange p) message]

  let t = if isBottom then Just $ Type.fromAST' AST.TypeBottom else Nothing
  return (AST.Panic (p, t))
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
annotateExceptionExprType t (AST.TryCatch p try pattern' catch) annotateType = do
  context <- get

  let exceptionT = exceptionType context
  when (null exceptionT) $ do
    let message = "expection type is not declared"
    tell [diagnostic Error EXCEPTION_TYPE_NOT_DECLARED (pointRange p) message]

  try' <- annotateType t try
  let t' = typeOf try'

  (pattern'', catch') <- case exceptionT of
    Just exceptionT' -> do
      (AST.AMatchCase _ pattern'' catch') <-
        annotateCaseType t' (AST.AMatchCase p pattern' catch) exceptionT' annotateType
      return (pattern'', catch')
    Nothing ->
      return (fmap (,Nothing) pattern', fmap (,Nothing) catch)

  return $ AST.TryCatch (p, t') try' pattern'' catch'
annotateExceptionExprType t (AST.TryWith p try catch) annotateType = do
  try' <- annotateType t try
  let t' = typeOf try'
  catch' <- annotateType t' catch
  return $ AST.TryWith (p, t') try' catch'
annotateExceptionExprType t (AST.TryCastAs p try type_ pattern' ok' with) annotateType = do
  try' <- annotateType Nothing try

  type' <- sanitizeT type_

  (AST.AMatchCase _ pattern'' ok'') <-
    annotateCaseType t (AST.AMatchCase p pattern' ok') type' annotateType

  with' <- annotateType (t <|> typeOf ok'') with

  let t' = t >> typeOf ok'' >> typeOf with'
  return $ AST.TryCastAs (p, t') try' (fmap (,Nothing) type_) pattern'' ok'' with'
annotateExceptionExprType _ _ _ = error "Unexpected non-exception expression"
