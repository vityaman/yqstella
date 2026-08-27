{-# LANGUAGE TupleSections #-}

module Type.Decl (withParamDecls, withDecls, toPair, toParamSilent) where

import Control.Monad (unless)
import Control.Monad.State (get)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Alias (typeAliasCollect, typeAliasResolve)
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type (..))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, tellD, validateUniqueBy, withStateTAE)
import Type.Expectation (sanitizeT, sanitizeTSilent, validateTypeParameters)

withParamDecls :: [AST.ParamDecl' Position] -> Context -> TypeAnnotationEnv Context
withParamDecls paramdecls context = do
  let toDiagnostic (AST.AParamDecl p (AST.StellaIdent parameterName) _) =
        let message = "duplicate parameter: " ++ parameterName
         in diagnostic Error DUPLICATE_FUNCTION_PARAMETER (pointRange p) message

  validated <- mapM (\parameter -> (parameter,) <$> toPair parameter) paramdecls
  unique <- validateUniqueBy True (name . fst) (toDiagnostic . fst) validated
  return $ foldr (uncurry Context.withTyped . snd) context unique
  where
    name (AST.AParamDecl _ (AST.StellaIdent value) _) = value

withTypeAliases :: [AST.Decl' Position] -> Context -> TypeAnnotationEnv Context
withTypeAliases decls context = do
  let types = typeAliasCollect decls

      visit (AST.DeclTypeAlias _ (AST.StellaIdent n) t) = fmap (n,) <$> typeAliasResolve types t
      visit _ = return Nothing

  typeAliases <- catMaybes <$> mapM visit decls
  return $ foldr (uncurry Context.withTypeAliased) context typeAliases

withDecls :: [AST.Decl' Position] -> Bool -> Context -> TypeAnnotationEnv Context
withDecls decls isTopLevel context = do
  context' <- withTypeAliases decls context
  funs <- withStateTAE (const context') (mapM visit decls)

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

  tellD $ fmap toDiagnostic duplicates
  return $ foldr (uncurry Context.withTyped) context' kvs
  where
    visit :: AST.Decl' Position -> TypeAnnotationEnv (Maybe (String, [(Position, Type)]))
    visit (AST.DeclFun p _ (AST.StellaIdent name) paramdecls (AST.SomeReturnType _ returntype) _ _ _) = do
      args'' <- mapM toParamSilent paramdecls
      let args' = fmap snd args''
      returntype' <- sanitizeT returntype
      return $ Just (name, [(p, Type.fn args' returntype')])
    visit (AST.DeclFun p _ (AST.StellaIdent name) _ (AST.NoReturnType _) _ _ _) = do
      tellD [notImplemented p $ "name resolution for DeclFun " ++ name ++ " due to implicit return type"]
      return Nothing
    visit (AST.DeclFunGeneric p _ (AST.StellaIdent name) parameters paramdecls (AST.SomeReturnType _ returntype) _ _ _) = do
      parameters' <- validateTypeParameters p parameters
      current <- get
      let (resolved, context') = Context.bindTypeVariables parameters' current
      args'' <- withStateTAE (const context') (mapM toParamSilent paramdecls)
      returntype' <- withStateTAE (const context') (sanitizeT returntype)
      let functionType = Type.fn (fmap snd args'') returntype'
          universalType = Type $ AST.TypeForAll () resolved (Type.toAST functionType)
      return $ Just (name, [(p, universalType)])
    visit (AST.DeclFunGeneric p _ (AST.StellaIdent name) _ _ (AST.NoReturnType _) _ _ _) = do
      tellD [notImplemented p $ "name resolution for DeclFunGeneric " ++ name ++ " due to implicit return type"]
      return Nothing
    visit (AST.DeclTypeAlias {}) =
      return Nothing
    visit (AST.DeclExceptionType p _) = do
      unless isTopLevel $ do
        let message = "only-top level exception type is allowed"
        tellD [diagnostic Error ILLEGAL_LOCAL_EXCEPTION_TYPE (pointRange p) message]
      return Nothing
    visit (AST.DeclExceptionVariant p _ _) = do
      unless isTopLevel $ do
        let message = "only-top level exception variant type is allowed"
        tellD [diagnostic Error ILLEGAL_LOCAL_OPEN_VARIANT_EXCEPTION (pointRange p) message]
      return Nothing

toPair :: AST.ParamDecl' Position -> TypeAnnotationEnv (String, Type)
toPair (AST.AParamDecl _ (AST.StellaIdent key) t) = do
  t' <- sanitizeT t
  return (key, t')

toParamSilent :: AST.ParamDecl' Position -> TypeAnnotationEnv (String, Type)
toParamSilent (AST.AParamDecl _ (AST.StellaIdent key) t) = do
  t' <- sanitizeTSilent t
  return (key, t')
