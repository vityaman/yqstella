module Type.Variant (variantFieldTyping, variantExprTyping) where

import Control.Monad.Writer (tell)
import Data.Foldable (find)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv)

variantFieldTyping ::
  Position ->
  String ->
  Maybe Type ->
  TypeAnnotationEnv (Maybe (AST.OptionalTyping' ()))
variantFieldTyping p tag (Just t'@(Type (AST.TypeVariant () entries))) = do
  let tag'' (AST.AVariantFieldType _ (AST.StellaIdent tag') _) = tag'
      entry = find (\x -> tag'' x == tag) entries
  case entry of
    Just (AST.AVariantFieldType _ _ t'') ->
      return $ Just t''
    Nothing -> do
      let message = "unexpected variant label " ++ tag ++ " for " ++ show t'
      tell [diagnostic Error UNEXPECTED_VARIANT_LABEL (pointRange p) message]
      return Nothing
variantFieldTyping p _ (Just t') = do
  let message = "expected " ++ show t' ++ ", but got a variant"
  tell [diagnostic Error UNEXPECTED_VARIANT (pointRange p) message]
  return Nothing
variantFieldTyping p _ Nothing = do
  let message = "type inference for variant types is not supported (use type ascriptions)"
  tell [diagnostic Error AMBIGUOUS_VARIANT_TYPE (pointRange p) message]
  return Nothing

variantExprTyping ::
  Maybe (AST.OptionalTyping' ()) ->
  AST.ExprData' Position ->
  TypeAnnotationEnv (Maybe Type)
variantExprTyping (Just (AST.NoTyping _)) (AST.NoExprData _) = do
  return $ Just (Type.fromAST' AST.TypeUnit)
variantExprTyping (Just (AST.SomeTyping _ t')) (AST.NoExprData p') = do
  let message = "expected some variant label with type " ++ show (Type t') ++ ", got nullary"
  tell [diagnostic Error MISSING_DATA_FOR_LABEL (pointRange p') message]
  return Nothing
variantExprTyping Nothing (AST.NoExprData _) = do
  return Nothing
variantExprTyping (Just (AST.NoTyping _)) (AST.SomeExprData p' _) = do
  let message = "expected nullary variant label"
  tell [diagnostic Error UNEXPECTED_DATA_FOR_NULLARY_LABEL (pointRange p') message]
  return Nothing
variantExprTyping (Just (AST.SomeTyping _ t')) (AST.SomeExprData _ _) = do
  return $ Just $ Type t'
variantExprTyping Nothing (AST.SomeExprData _ _) = do
  return Nothing
