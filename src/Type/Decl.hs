module Type.Decl (withParamDecls, withDecls, toPair) where

import Control.Monad.Writer (tell)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Severity (..), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import Misc.Duplicate (sepUniqDupBy)
import qualified SyntaxGen.AbsStella as AST
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type)
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv)
import Type.Expectation (sanitizeT)

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

withDecls :: [AST.Decl' Position] -> Context -> TypeAnnotationEnv Context
withDecls decls context = do
  decls' <- mapM visit decls

  let kpvs = catMaybes decls'

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
  return $ foldr (uncurry Context.withTyped) context kvs
  where
    visit :: AST.Decl' Position -> TypeAnnotationEnv (Maybe (String, [(Position, Type)]))
    visit (AST.DeclFun p _ (AST.StellaIdent name) paramdecls (AST.SomeReturnType _ returntype) _ _ _) = do
      args'' <- mapM toPair paramdecls
      let args' = fmap snd args''

      returntype' <- Type.fromAST <$> sanitizeT returntype

      return $ Just (name, [(p, Type.fn args' returntype')])
    visit (AST.DeclFun p _ (AST.StellaIdent name) _ (AST.NoReturnType _) _ _ _) = do
      tell [notImplemented p $ "name resolution for DeclFun " ++ name ++ " due to implicit return type"]
      return Nothing
    visit (AST.DeclFunGeneric p _ (AST.StellaIdent name) _ _ _ _ _ _) = do
      tell [notImplemented p $ "name resolution for DeclFunGeneric " ++ name]
      return Nothing
    visit (AST.DeclTypeAlias p (AST.StellaIdent name) _) = do
      tell [notImplemented p $ "name resolution for DeclTypeAlias " ++ name]
      return Nothing
    visit (AST.DeclExceptionType p type_) = do
      tell [notImplemented p $ "name resolution for DeclExceptionType " ++ show (Type.fromAST type_)]
      return Nothing
    visit (AST.DeclExceptionVariant p (AST.StellaIdent name) _) = do
      tell [notImplemented p $ "name resolution for DeclExceptionVariant " ++ show name]
      return Nothing

toPair :: AST.ParamDecl' Position -> TypeAnnotationEnv (String, Type)
toPair (AST.AParamDecl _ (AST.StellaIdent key) t) = do
  t' <- sanitizeT t
  return (key, Type.fromAST t')
