{-# LANGUAGE TupleSections #-}

module Type.Reference (annotateRefExprType) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (tell)
import Diagnostic.Core (notImplemented)
import Diagnostic.Position (Position)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf)

annotateRefExprType ::
  Maybe Type ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateRefExprType t (AST.Sequence p prv nxt) annotateType = do
  prv' <- annotateType (Just $ Type.fromAST' AST.TypeUnit) prv
  nxt' <- annotateType t nxt
  return (AST.Sequence (p, typeOf prv' >> typeOf nxt') prv' nxt')
annotateRefExprType _ x@(AST.Assign {}) _ = do
  tell [notImplemented (annotation x) "Assign"]
  return $ fmap (,Nothing) x
annotateRefExprType _ x@(AST.Ref {}) _ = do
  tell [notImplemented (annotation x) "Ref"]
  return $ fmap (,Nothing) x
annotateRefExprType _ x@(AST.Deref {}) _ = do
  tell [notImplemented (annotation x) "Deref"]
  return $ fmap (,Nothing) x
annotateRefExprType _ x@(AST.ConstMemory {}) _ = do
  tell [notImplemented (annotation x) "ConstMemory"]
  return $ fmap (,Nothing) x
annotateRefExprType _ _ _ = error "Unexpected non-reference expression"
