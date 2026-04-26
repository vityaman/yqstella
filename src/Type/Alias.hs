module Type.Alias (typeAliasCollect, typeAliasResolve) where

import Control.Monad.Writer (MonadWriter (tell))
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
import Type.Env (TypeAnnotationEnv)
import Type.Expectation (sanitizeT)

type TypeAliasesRaw = Map String (AST.Type' Position)

typeAliasCollect :: [AST.Decl' Position] -> TypeAliasesRaw
typeAliasCollect = Map.fromList . mapMaybe toTypeAlias
  where
    toTypeAlias (AST.DeclTypeAlias _ (AST.StellaIdent n) t) = Just (n, t)
    toTypeAlias _ = Nothing

typeAliasResolve :: TypeAliasesRaw -> AST.Type' Position -> TypeAnnotationEnv (Maybe Type)
typeAliasResolve types t = do
  t' <- go mempty t
  mapM sanitizeT t'
  where
    go :: Set String -> AST.Type' Position -> TypeAnnotationEnv (Maybe (AST.Type' Position))
    go vis (AST.TypeFun p args returntype) = do
      args' <- sequence <$> mapM (go vis) args
      returntype' <- go vis returntype
      return $ AST.TypeFun p <$> args' <*> returntype'
    go vis (AST.TypeForAll p id' body) = do
      body' <- go vis body
      return $ AST.TypeForAll p id' <$> body'
    go vis (AST.TypeRec p id' body) = do
      body' <- go vis body
      return $ AST.TypeRec p id' <$> body'
    go vis (AST.TypeSum p lhs rhs) = do
      lhs' <- go vis lhs
      rhs' <- go vis rhs
      return $ AST.TypeSum p <$> lhs' <*> rhs'
    go vis (AST.TypeTuple p types') = do
      types'' <- sequence <$> mapM (go vis) types'
      return $ AST.TypeTuple p <$> types''
    go vis (AST.TypeRecord p fields) = do
      let fieldGo (AST.ARecordFieldType p' (AST.StellaIdent label) ty) = do
            ty' <- go vis ty
            return $ AST.ARecordFieldType p' (AST.StellaIdent label) <$> ty'
      fields' <- sequence <$> mapM fieldGo fields
      return $ AST.TypeRecord p <$> fields'
    go vis (AST.TypeVariant p fields) = do
      let fieldGo (AST.AVariantFieldType p' (AST.StellaIdent label) (AST.NoTyping p'')) =
            return $ Just $ AST.AVariantFieldType p' (AST.StellaIdent label) (AST.NoTyping p'')
          fieldGo (AST.AVariantFieldType p' (AST.StellaIdent label) (AST.SomeTyping p'' t')) = do
            ty' <- go vis t'
            return (AST.AVariantFieldType p' (AST.StellaIdent label) . AST.SomeTyping p'' <$> ty')
      fields' <- sequence <$> mapM fieldGo fields
      return $ AST.TypeVariant p <$> fields'
    go vis (AST.TypeList p elemTy) = do
      elemTy' <- go vis elemTy
      return $ AST.TypeList p <$> elemTy'
    go vis (AST.TypeRef p ty) = do
      ty' <- go vis ty
      return $ AST.TypeRef p <$> ty'
    go vis (AST.TypeVar p (AST.StellaIdent name)) =
      case Map.lookup name types of
        Just ty
          | name `Set.member` vis -> do
              let msg = "recursive type alias detected for " ++ name
              tell [diagnostic Error UNDEFINED_TYPE_VARIABLE (pointRange p) msg]
              return Nothing
          | otherwise -> go (Set.insert name vis) ty
        Nothing -> do
          let message = "undefined type alias " ++ name
          tell [diagnostic Error UNDEFINED_TYPE_VARIABLE (pointRange p) message]
          return Nothing
    go _ x = return $ Just x
