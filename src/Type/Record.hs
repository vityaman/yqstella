{-# LANGUAGE TupleSections #-}

module Type.Record (annotateDotRecordType, annotateRecordType) where

import Control.Monad (unless, when)
import Control.Monad.Writer
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import Misc.Duplicate (sepUniqDupBy)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf)

annotateDotRecordType ::
  Maybe Type ->
  Position ->
  AST.Expr' Position ->
  String ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateDotRecordType t p expr field annotateType = do
  expr' <- annotateType Nothing expr

  t' <- case typeOf expr' of
    Just (Type (AST.TypeRecord _ fields)) -> do
      -- TODO(vityaman): make commons for records
      let toKV (AST.ARecordFieldType () (AST.StellaIdent k) v) = (k, Type v)
          tmap = Map.fromList $ fmap toKV fields

          t' = Map.lookup field tmap

      when (null t') $
        let message = "missing record field " ++ field ++ " : " ++ maybe "?" show t
         in tell [diagnostic Error UNEXPECTED_FIELD_ACCESS (pointRange p) message]

      return t'
    Just actual -> do
      let message =
            "type mismatch: expected record with "
              ++ (field ++ ": " ++ maybe "?" show t)
              ++ (", got " ++ show actual)
      tell [diagnostic Error NOT_A_RECORD (pointRange p) message]
      return Nothing
    Nothing ->
      return Nothing

  return (AST.DotRecord (p, t') expr' (AST.StellaIdent field))

annotateRecordType ::
  Maybe Type ->
  Position ->
  [AST.Binding' Position] ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateRecordType t p bindings annotateType = do
  let name (AST.ABinding _ (AST.StellaIdent name') _) = name'

      toDiagnostic (AST.ABinding p' (AST.StellaIdent name') _) =
        let message = "duplicate field: " ++ name'
         in diagnostic Error DUPLICATE_RECORD_FIELDS (pointRange p') message

      toMap :: Maybe Type -> Maybe (Map String Type)
      toMap (Just (Type (AST.TypeRecord () fields))) =
        let toKV (AST.ARecordFieldType () (AST.StellaIdent k) v) = (k, Type v)
         in Just $ Map.fromList $ fmap toKV fields
      toMap _ = Nothing

      toMap' :: [AST.Binding' (a, Maybe Type)] -> Maybe (Map String Type)
      toMap' bindingsUniq = Map.fromList <$> traverse toKV bindingsUniq
        where
          toKV (AST.ABinding (_, t') (AST.StellaIdent name') _) = fmap (name',) t'

      lookup' :: String -> Maybe (Map String Type) -> Maybe Type
      lookup' _ Nothing = Nothing
      lookup' k (Just m) = Map.lookup k m

      annotateType' t'' (AST.ABinding p' name' expr) = do
        expr' <- annotateType t'' expr
        let t' = typeOf expr'
        return (AST.ABinding (p', t') name' expr')

      annotateType'' tmap' x =
        annotateType' (lookup' (name x) tmap') x

  let (bindingsUniq, bindingsDup) = sepUniqDupBy name bindings
      expectedTMap = toMap t

  bindingsUniq' <- mapM (annotateType'' expectedTMap) bindingsUniq
  bindingsDup' <- mapM (annotateType'' expectedTMap) bindingsDup
  let bindings' = bindingsUniq' ++ bindingsDup'

  tell $ fmap toDiagnostic bindingsDup

  let actualTMap = toMap' bindingsUniq'

  t'' <- case (expectedTMap, actualTMap) of
    (Just expected, Just actual) -> do
      let missing = Map.keys $ Map.difference expected actual
      unless (null missing) $
        let message = "missing record fields: " ++ intercalate ", " missing
         in tell [diagnostic Error MISSING_RECORD_FIELDS (pointRange p) message]

      let unexpected = Map.keys $ Map.difference actual expected
      unless (null unexpected) $
        let message = "unexpected record fields: " ++ intercalate ", " missing
         in tell [diagnostic Error UNEXPECTED_RECORD_FIELDS (pointRange p) message]

      return $
        if null missing && null unexpected
          then Just expected
          else Nothing
    (Nothing, Just t') ->
      return $ Just t'
    (_, Nothing) ->
      return Nothing

  let toType map' =
        let xs = [AST.ARecordFieldType () (AST.StellaIdent k) v | (k, Type v) <- map']
         in Type $ AST.TypeRecord () xs

      t' = fmap (toType . Map.toList) t''

  return (AST.Record (p, t') bindings')
