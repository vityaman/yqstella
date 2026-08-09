{-# LANGUAGE TupleSections #-}

module Type.Application (annotateAbstractionType, annotateApplicationType) where

import Annotation (Annotated (annotation))
import Control.Applicative (Alternative ((<|>)))
import qualified Control.Arrow as Data.Bifunctor
import Control.Monad (guard, when, zipWithM)
import Control.Monad.State (get)
import Control.Monad.Writer
import Data.Maybe (fromMaybe, mapMaybe)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Decl (toParamSilent, withParamDecls)
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf, withStateTAE)
import Type.Expectation (mismatchSS)
import Type.Lift (liftType')

annotateAbstractionType ::
  Maybe Type ->
  Position ->
  [AST.ParamDecl' Position] ->
  AST.Expr' Position ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateAbstractionType t p paramdecls expr annotateType = do
  let infer' expr'' = do
        context' <- get >>= withParamDecls paramdecls
        expr' <- withStateTAE (const context') (annotateType Nothing expr'')
        argtypes <- Type.fn . fmap snd <$> mapM toParamSilent paramdecls
        return (fmap argtypes (typeOf expr'), expr')

  (t', expr') <- case t of
    Just t'@(Type (AST.TypeFun () argtypes returntype)) -> do
      paramdecls' <- mapM toParamSilent paramdecls

      let actual = Data.Bifunctor.first annotation <$> zip paramdecls paramdecls'
          expected = fmap Type argtypes

      let toDiagnostic ((p', (name, actual')), expected') = do
            guard $ actual' /= expected'
            let m = "(" ++ name ++ " : " ++ show actual' ++ ")"
            return $ mismatchSS UNEXPECTED_TYPE_FOR_PARAMETER p' (show expected') m

      let actualLen = length actual
          expectedLen = length expected
      when (actualLen /= expectedLen) $ do
        let message =
              "expected "
                ++ show expectedLen
                ++ " parameters for type "
                ++ show t'
                ++ ", but actually got "
                ++ show actualLen
        tell [diagnostic Error UNEXPECTED_NUMBER_OF_PARAMETERS_IN_LAMBDA (pointRange p) message]
        return ()

      tell $ mapMaybe toDiagnostic (zip actual expected)

      context' <- get >>= withParamDecls paramdecls
      expr' <- withStateTAE (const context') (annotateType (Just $ Type returntype) expr)

      return (Just t', expr')
    Just t'' -> do
      (t', expr') <- infer' expr
      tell [mismatchSS UNEXPECTED_LAMBDA p (show t'') (maybe "lambda" show t')]
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
  let (f'position, f't) = annotation f'

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
            tell [diagnostic Error INCORRECT_NUMBER_OF_ARGUMENTS (pointRange p) message]
            return Nothing
          else
            return $ Just returntype'

      return (xs', returntype'')
    Just actual -> do
      let message = "type mismatch: expected a function, got " ++ show actual
       in tell [diagnostic Error NOT_A_FUNCTION (pointRange f'position) message]

      xs' <- mapM (annotateType Nothing) xs

      let unknown = Type.fromAST' AST.TypeAuto
          expectedArgTypes = fmap (fromMaybe unknown . typeOf) xs'
          expected = Type.fn expectedArgTypes unknown

      let message = "note: expected " ++ show expected
       in tell [diagnostic Error NOT_A_FUNCTION (pointRange f'position) message]

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
