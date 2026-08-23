{-# LANGUAGE TupleSections #-}

module Type.Application (annotateAbstractionType, annotateApplicationType) where

import Annotation (Annotated (annotation))
import Control.Applicative (Alternative ((<|>)))
import qualified Control.Arrow as Data.Bifunctor
import Control.Monad (when, zipWithM)
import Control.Monad.State (gets)
import Data.Maybe (fromMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Constraint as Constraint
import Type.Context (withName, withTyped)
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Decl (toPair)
import Type.Env (TypeAnnotationEnv, TypeAnnotator, freshTypeVar, positionOf, tellC, tellD, typeOf, withStateTAE)
import Type.Expectation (ensureEqParamType, mismatchSS)
import Type.Lift (liftType')

annotateAbstractionType ::
  Maybe Type ->
  Position ->
  [AST.ParamDecl' Position] ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateAbstractionType t p paramdecls expr annotateType = do
  paramdecls' <- mapM toPair paramdecls

  let withParams context =
        foldr (uncurry withTyped) (withName ("abstraction at " ++ show p) context) paramdecls'

  let infer' expr'' = do
        context' <- gets withParams
        expr' <- withStateTAE (const context') (annotateType Nothing expr'')
        let abstractionType = Type.fn (fmap snd paramdecls')
        return (fmap abstractionType (typeOf expr'), expr')

  let actual = Data.Bifunctor.first annotation <$> zip paramdecls paramdecls'
      actualLen = length actual

  t'' <- case t of
    Just v@(Type (AST.TypeVar () _)) -> do
      (Type returnT) <- freshTypeVar
      let functionT = Type $ AST.TypeFun () (fmap (Type.toAST . snd . snd) actual) returnT
      tellC [Constraint.Eq p v functionT]
      return $ Just functionT
    x -> return x

  (t', expr') <- case t'' of
    Just t'@(Type (AST.TypeFun () argtypes returntype)) -> do
      let expected = fmap Type argtypes
          expectedLen = length expected

      when (actualLen /= expectedLen) $ do
        let message =
              "expected "
                ++ show expectedLen
                ++ " parameters for type "
                ++ show t'
                ++ ", but actually got "
                ++ show actualLen
        tellD [diagnostic Error UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA (pointRange p) message]
        return ()

      let ensureParameterType ((p', (name, actual')), expected') =
            ensureEqParamType p' name actual' expected'
      mapM_ ensureParameterType (zip actual expected)

      context' <- gets withParams
      expr' <- withStateTAE (const context') (annotateType (Just $ Type returntype) expr)

      return (Just t', expr')
    Just (Type (AST.TypeTop ())) ->
      infer' expr
    Just t''' -> do
      (t', expr') <- infer' expr
      tellD [mismatchSS UNEXPECTED_LAMBDA p (show t''') (maybe "lambda" show t')]
      return (t', expr')
    Nothing ->
      infer' expr

  return $ AST.Abstraction (p, t') (fmap (fmap (,Nothing)) paramdecls) expr'

annotateApplicationType ::
  Maybe Type ->
  Position ->
  AST.Expr' Position ->
  [AST.Expr' Position] ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateApplicationType t p f xs annotateType = do
  f' <- annotateType Nothing f
  let f'position = positionOf f'

  f't <- case typeOf f' of
    Just variable@(Type (AST.TypeVar () _)) -> do
      argTypes <- fmap Type.toAST <$> mapM (const freshTypeVar) [1 .. length xs]
      returnType <- Type.toAST <$> freshTypeVar
      let fType = Type (AST.TypeFun () argTypes returnType)
      tellC [Constraint.Eq p variable fType]
      return $ Just fType
    x -> return x

  (xs', type') <- case f't of
    Just (Type (AST.TypeFun _ argTypes returntype)) -> do
      let argtypes' = fmap Type argTypes
          returntype' = Type returntype

      xs' <- zipWithM annotateType (fmap Just argtypes') xs

      let expectedLen = length argtypes'
          actualLen = length xs

      returntype'' <-
        if expectedLen /= actualLen
          then do
            let message = "expected " ++ show expectedLen ++ " arguments, got " ++ show actualLen
            tellD [diagnostic Error INCORRECT_NUMBER_OF_ARGUMENTS (pointRange p) message]
            return Nothing
          else
            return $ Just returntype'

      return (xs', returntype'')
    Just actual -> do
      let message = "type mismatch: expected a function, got " ++ show actual
       in tellD [diagnostic Error NOT_A_FUNCTION (pointRange f'position) message]

      xs' <- mapM (annotateType Nothing) xs

      let unknown = Type.fromAST' AST.TypeAuto
          expectedArgTypes = fmap (fromMaybe unknown . typeOf) xs'
          expected = Type.fn expectedArgTypes unknown

      let message = "note: expected " ++ show expected
       in tellD [diagnostic Error NOT_A_FUNCTION (pointRange f'position) message]

      return (xs', Nothing)
    Nothing -> do
      xs' <- mapM (annotateType Nothing) xs
      return (xs', Nothing)

  _ <- case (t, type') of
    (Just expected, Just actual) -> do
      _ <- liftType' p actual (Just expected)
      return ()
    _ ->
      return ()

  return $ AST.Application (p, t <|> type') f' xs'
