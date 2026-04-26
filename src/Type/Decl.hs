{-# LANGUAGE TupleSections #-}

module Type.Decl (withParamDecls, withDecls, toPair, toParamSilent) where

import Control.Monad.Writer (tell)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import Misc.Duplicate (sepUniqDupBy)
import qualified SyntaxGen.AbsStella as AST
import Type.Alias (typeAliasCollect, typeAliasResolve)
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type (..))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, withStateTAE)
import Type.Expectation (sanitizeT, sanitizeTSilent)

withParamDecls :: [AST.ParamDecl' Position] -> Context -> TypeAnnotationEnv Context
withParamDecls paramdecls context = do
  let (uniq, dup) = sepUniqDupBy (\(AST.AParamDecl _ (AST.StellaIdent n) _) -> n) paramdecls

      toDiagnostic (AST.AParamDecl p (AST.StellaIdent name) _) =
        let message = "duplicate parameter: " ++ name
         in diagnostic Error DUPLICATE_FUNCTION_PARAMETER (pointRange p) message

  paramdecls' <- mapM toPair uniq
  mapM_ toPair dup

  tell $ fmap toDiagnostic dup
  return $ foldr (uncurry Context.withTyped) context paramdecls'

withTypeAliases :: [AST.Decl' Position] -> Context -> TypeAnnotationEnv Context
withTypeAliases decls context = do
  let types = typeAliasCollect decls

      visit (AST.DeclTypeAlias _ (AST.StellaIdent n) t) = fmap (n,) <$> typeAliasResolve types t
      visit _ = return Nothing

  typeAliases <- catMaybes <$> mapM visit decls
  return $ foldr (uncurry Context.withTypeAliased) context typeAliases

withDecls :: [AST.Decl' Position] -> Context -> TypeAnnotationEnv Context
withDecls decls context = do
  context' <- withTypeAliases decls context
  funs <- withStateTAE (const context') (mapM visitFuns decls)

  let kpvs = catMaybes funs

      duplicates =
        [ (name, position)
          | (name, pvs) <- Map.toList $ Map.fromListWith (++) kpvs,
            1 < length pvs,
            (position, _) <- pvs
        ]

      toDiagnostic (name, p) =
        let message = "duplicate declaration: " ++ name
         in diagnostic Error DUPLICATE_FUNCTION_DECLARATION (pointRange p) message

      kvs = fmap unpack kpvs
      unpack (k, [(_, t)]) = (k, t)
      unpack _ = undefined

  tell $ fmap toDiagnostic duplicates
  return $ foldr (uncurry Context.withTyped) context' kvs
  where
    visitFuns :: AST.Decl' Position -> TypeAnnotationEnv (Maybe (String, [(Position, Type)]))
    visitFuns (AST.DeclFun p _ (AST.StellaIdent name) paramdecls (AST.SomeReturnType _ returntype) _ _ _) = do
      args'' <- mapM toParamSilent paramdecls
      let args' = fmap snd args''
      returntype' <- sanitizeT returntype
      return $ Just (name, [(p, Type.fn args' returntype')])
    visitFuns (AST.DeclFun p _ (AST.StellaIdent name) _ (AST.NoReturnType _) _ _ _) = do
      tell [notImplemented p $ "name resolution for DeclFun " ++ name ++ " due to implicit return type"]
      return Nothing
    visitFuns (AST.DeclFunGeneric p _ (AST.StellaIdent name) _ _ _ _ _ _) = do
      tell [notImplemented p $ "name resolution for DeclFunGeneric " ++ name]
      return Nothing
    visitFuns (AST.DeclTypeAlias {}) = do
      return Nothing
    visitFuns (AST.DeclExceptionType p type_) = do
      tell [notImplemented p $ "name resolution for DeclExceptionType " ++ show (Type.fromAST type_)]
      return Nothing
    visitFuns (AST.DeclExceptionVariant p (AST.StellaIdent name) _) = do
      tell [notImplemented p $ "name resolution for DeclExceptionVariant " ++ show name]
      return Nothing

-- | 'sanitize' the parameter type for the typing environment. Duplicate
-- record\/variant fields in the written type are diagnosed here. The annotation
-- pass (see 'Type.Annotation', 'toParamSilent') re-shapes the same syntax again
-- without emitting those diagnostics a second time.
toPair :: AST.ParamDecl' Position -> TypeAnnotationEnv (String, Type)
toPair (AST.AParamDecl _ (AST.StellaIdent key) t) = do
  t' <- sanitizeT t
  return (key, t')

-- | Like 'toPair' but does not re-report duplicate record\/variant type fields.
-- Used in 'visitFuns' (param types are later checked with 'toPair' in
-- 'withParamDecls'), in 'Type.Annotation' and 'Type.Application' after the
-- context pass.
toParamSilent :: AST.ParamDecl' Position -> TypeAnnotationEnv (String, Type)
toParamSilent (AST.AParamDecl _ (AST.StellaIdent key) t) = do
  t' <- sanitizeTSilent t
  return (key, t')
