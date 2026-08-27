module Type.Alias (typeAliasCollect, typeAliasResolve) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (..))
import Type.Env (TypeAnnotationEnv, tellD)
import Type.Expectation (sanitizeT)

type TypeAliasesRaw = Map String (AST.Type' Position)

typeAliasCollect :: [AST.Decl' Position] -> TypeAliasesRaw
typeAliasCollect = Map.fromList . mapMaybe toTypeAlias
  where
    toTypeAlias (AST.DeclTypeAlias _ (AST.StellaIdent n) t) = Just (n, t)
    toTypeAlias _ = Nothing

typeAliasResolve :: TypeAliasesRaw -> AST.Type' Position -> TypeAnnotationEnv (Maybe Type)
typeAliasResolve aliases type_ = do
  resolved <- resolve mempty mempty type_
  mapM sanitizeT resolved
  where
    resolve :: Set String -> Set String -> AST.Type' Position -> TypeAnnotationEnv (Maybe (AST.Type' Position))
    resolve expandingAliases boundVariables (AST.TypeFun p args returntype) = do
      args' <- sequence <$> mapM (resolve expandingAliases boundVariables) args
      returntype' <- resolve expandingAliases boundVariables returntype
      return $ AST.TypeFun p <$> args' <*> returntype'
    resolve expandingAliases boundVariables (AST.TypeForAll p parameters body) = do
      body' <- resolve expandingAliases (boundVariables <> identNames parameters) body
      return $ AST.TypeForAll p parameters <$> body'
    resolve expandingAliases boundVariables (AST.TypeRec p parameter body) = do
      body' <- resolve expandingAliases (Set.insert (identName parameter) boundVariables) body
      return $ AST.TypeRec p parameter <$> body'
    resolve expandingAliases boundVariables (AST.TypeSum p lhs rhs) = do
      lhs' <- resolve expandingAliases boundVariables lhs
      rhs' <- resolve expandingAliases boundVariables rhs
      return $ AST.TypeSum p <$> lhs' <*> rhs'
    resolve expandingAliases boundVariables (AST.TypeTuple p types) = do
      types' <- sequence <$> mapM (resolve expandingAliases boundVariables) types
      return $ AST.TypeTuple p <$> types'
    resolve expandingAliases boundVariables (AST.TypeRecord p fields) = do
      let resolveRecordField (AST.ARecordFieldType fieldPosition label fieldType) = do
            fieldType' <- resolve expandingAliases boundVariables fieldType
            return $ AST.ARecordFieldType fieldPosition label <$> fieldType'
      fields' <- sequence <$> mapM resolveRecordField fields
      return $ AST.TypeRecord p <$> fields'
    resolve expandingAliases boundVariables (AST.TypeVariant p fields) = do
      let resolveVariantField field@(AST.AVariantFieldType _ _ (AST.NoTyping _)) = return $ Just field
          resolveVariantField (AST.AVariantFieldType fieldPosition label (AST.SomeTyping typingPosition fieldType)) = do
            fieldType' <- resolve expandingAliases boundVariables fieldType
            return (AST.AVariantFieldType fieldPosition label . AST.SomeTyping typingPosition <$> fieldType')
      fields' <- sequence <$> mapM resolveVariantField fields
      return $ AST.TypeVariant p <$> fields'
    resolve expandingAliases boundVariables (AST.TypeList p item) = do
      item' <- resolve expandingAliases boundVariables item
      return $ AST.TypeList p <$> item'
    resolve expandingAliases boundVariables (AST.TypeRef p referenced) = do
      referenced' <- resolve expandingAliases boundVariables referenced
      return $ AST.TypeRef p <$> referenced'
    resolve expandingAliases boundVariables typeVariable@(AST.TypeVar p (AST.StellaIdent typeName))
      | typeName `Set.member` boundVariables = return $ Just typeVariable
      | otherwise = case Map.lookup typeName aliases of
          Just alias
            | typeName `Set.member` expandingAliases -> do
                let message = "recursive type alias detected for " ++ typeName
                tellD [diagnostic Error UNDEFINED_TYPE_VARIABLE (pointRange p) message]
                return Nothing
            | otherwise -> resolve (Set.insert typeName expandingAliases) boundVariables alias
          Nothing -> do
            let message = "undefined type alias " ++ typeName
            tellD [diagnostic Error UNDEFINED_TYPE_VARIABLE (pointRange p) message]
            return Nothing
    resolve _ _ other = return $ Just other

    identNames = Set.fromList . fmap identName
    identName (AST.StellaIdent value) = value
