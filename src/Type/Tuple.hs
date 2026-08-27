module Type.Tuple (annotateDotTupleType, annotateTupleType) where

import Control.Monad (zipWithM)
import Control.Monad.State (gets)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Constraint as Constraint
import qualified Type.Context as Context
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, freshTypeVar, tellC, tellD, typeOf)
import Type.Lift (liftType)
import qualified Type.Unification as Unification

annotateDotTupleType ::
  Maybe Type ->
  Position ->
  AST.Expr' Position ->
  Integer ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateDotTupleType t p expr index annotateType = do
  expr' <- annotateType Nothing expr
  metaVariables <- gets Context.metaVars

  t' <- case typeOf expr' of
    _ | index == 0 -> do
      let message = "tuple index should be positive, got 0"
      tellD [diagnostic Error TUPLE_INDEX_OUT_OF_BOUNDS (pointRange p) message]
      return Nothing
    Just actual@(Type (AST.TypeTuple _ ts)) | length ts < fromInteger index -> do
      let message =
            "type mismatch: expected tuple "
              ++ ("with size at least " ++ show index)
              ++ (", got " ++ show actual)
      tellD [diagnostic Error TUPLE_INDEX_OUT_OF_BOUNDS (pointRange p) message]
      return Nothing
    Just (Type (AST.TypeTuple _ ts)) -> do
      let actual = ts !! fromInteger (index - 1)
      t' <- liftType p (const actual) t
      return $ Just t'
    Just variable@(Type (AST.TypeVar () _))
      | Unification.isMetaVar metaVariables variable,
        index == 1 || index == 2 -> do
          (Type lhsT) <- freshTypeVar
          (Type rhsT) <- freshTypeVar
          tellC [Constraint.Eq p variable (Type $ AST.TypeTuple () [lhsT, rhsT])]
          if index == 1
            then return $ Just $ Type lhsT
            else return $ Just $ Type rhsT
      | Unification.isMetaVar metaVariables variable -> do
          let message = "tuple type reconstruction is not yet implemented"
          tellD [diagnostic Error NOT_IMPLEMENTED (pointRange p) message]
          return Nothing
    Just actual -> do
      let message = "type mismatch: expected tuple, got " ++ show actual
      tellD [diagnostic Error NOT_A_TUPLE (pointRange p) message]
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
    Just (Type (AST.TypeTop ())) -> do
      exprs' <- mapM (annotateType Nothing) exprs
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
      tellD [diagnostic Error code (pointRange p) message]

      return (exprs', False)
    Nothing -> do
      exprs' <- mapM (annotateType Nothing) exprs
      return (exprs', True)

  let t' = Type . AST.TypeTuple () <$> traverse (fmap Type.toAST . typeOf) exprs'

  return (AST.Tuple (p, if isReliable then t' else Nothing) exprs')
