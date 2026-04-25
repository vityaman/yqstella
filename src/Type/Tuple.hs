module Type.Tuple (annotateDotTupleType, annotateTupleType) where

import Control.Monad (zipWithM)
import Control.Monad.Writer
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf)
import Type.Expectation (liftType)

annotateDotTupleType ::
  Maybe Type ->
  Position ->
  AST.Expr' Position ->
  Integer ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateDotTupleType t p expr index annotateType = do
  expr' <- annotateType Nothing expr

  t' <- case typeOf expr' of
    _ | index == 0 -> do
      let message = "tuple index should be positive, got 0"
      tell [diagnostic Error TUPLE_INDEX_OUT_OF_BOUNDS (pointRange p) message]
      return Nothing
    Just actual@(Type (AST.TypeTuple _ ts)) | length ts < fromInteger index -> do
      let message =
            "type mismatch: expected tuple "
              ++ ("with size at least " ++ show index)
              ++ (", got " ++ show actual)
      tell [diagnostic Error TUPLE_INDEX_OUT_OF_BOUNDS (pointRange p) message]
      return Nothing
    Just (Type (AST.TypeTuple _ ts)) -> do
      let actual = ts !! fromInteger (index - 1)
      t' <- liftType p (const actual) t
      return $ Just t'
    Just actual -> do
      let message = "type mismatch: expected tuple, got " ++ show actual
      tell [diagnostic Error NOT_A_TUPLE (pointRange p) message]
      return Nothing
    Nothing ->
      return Nothing

  return (AST.DotTuple (p, t') expr' index)

annotateTupleType ::
  Maybe Type ->
  Position ->
  [AST.Expr' Position] ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateTupleType t p exprs annotateType = do
  (exprs', isReliable) <- case t of
    Just (Type (AST.TypeTuple _ ts)) | length ts == length exprs -> do
      exprs' <- zipWithM annotateType (fmap (Just . Type) ts) exprs
      return (exprs', True)
    Just expected -> do
      exprs' <- mapM (annotateType Nothing) exprs

      let code = case expected of
            (Type (AST.TypeTuple _ _)) -> UNEXPECTED_TUPLE_LENGTH
            _ -> UNEXPECTED_TUPLE

      let unknown = AST.TypeAuto ()
          actualTypes = fmap (maybe unknown Type.toAST . typeOf) exprs'
          actual = Type.fromAST $ AST.TypeTuple () actualTypes

      let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
      tell [diagnostic Error code (pointRange p) message]

      return (exprs', False)
    Nothing -> do
      exprs' <- mapM (annotateType Nothing) exprs
      return (exprs', True)

  let t' = Type . AST.TypeTuple () <$> traverse (fmap Type.toAST . typeOf) exprs'

  return (AST.Tuple (p, if isReliable then t' else Nothing) exprs')
