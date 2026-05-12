{-# LANGUAGE TupleSections #-}

module Type.Reference (annotateRefExprType) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (tell)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (..))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf)
import Type.Expectation (liftType)

annotateRefExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateRefExprType t (AST.Sequence p prv nxt) annotateType = do
  prv' <- annotateType (Just $ Type.fromAST' AST.TypeUnit) prv
  nxt' <- annotateType t nxt
  return (AST.Sequence (p, typeOf prv' >> typeOf nxt') prv' nxt')
annotateRefExprType t (AST.Assign p dst src) annotateType = do
  _ <- liftType p AST.TypeUnit t

  dst' <- annotateType Nothing dst
  src'expected't <- case typeOf dst' of
    Just (Type (AST.TypeRef () t')) ->
      return $ Just $ Type.fromAST t'
    Just t' -> do
      let message = "expected a reference, but got " ++ show t'
      tell [diagnostic Error NOT_A_REFERENCE (pointRange p) message]
      return Nothing
    Nothing ->
      return Nothing
  src' <- annotateType src'expected't src

  return (AST.Assign (p, typeOf src' >> typeOf dst' >> Just (Type $ AST.TypeUnit ())) dst' src')
annotateRefExprType Nothing (AST.Ref p expr) annotateType = do
  expr' <- annotateType Nothing expr
  let t = Type . AST.TypeRef () . (\(Type x) -> x) <$> typeOf expr'
  return (AST.Ref (p, t) expr')
annotateRefExprType (Just t'@(Type (AST.TypeRef () t))) (AST.Ref p expr) annotateType = do
  let expr't = Type.fromAST t
  expr' <- annotateType (Just expr't) expr
  return (AST.Ref (p, typeOf expr' >> Just t') expr')
annotateRefExprType (Just t) (AST.Ref p expr) annotateType = do
  expr' <- annotateType Nothing expr
  let message = "expected reference type, but got " ++ show t
  tell [diagnostic Error UNEXPECTED_REFERENCE (pointRange p) message]
  return (AST.Ref (p, Nothing) expr')
annotateRefExprType t (AST.Deref p expr) annotateType = do
  let ref't = Type . AST.TypeRef () . (\(Type x) -> x) <$> t
  expr' <- annotateType ref't expr

  t' <- case typeOf expr' of
    Just t'@(Type (AST.TypeRef () _)) ->
      return $ Just t'
    Just t'@(Type _) -> do
      let message = "expected a reference type, got " ++ show t'
      tell [diagnostic Error NOT_A_REFERENCE (pointRange p) message]
      return Nothing
    Nothing ->
      return Nothing

  return (AST.Deref (p, t') expr')
annotateRefExprType _ x@(AST.ConstMemory {}) _ = do
  tell [notImplemented (annotation x) "ConstMemory"]
  return $ fmap (,Nothing) x
annotateRefExprType _ _ _ = error "Unexpected non-reference expression"
