module Type.Unification (unify) where

import qualified Data.Set as Set
import Diagnostic.Code (Code (OCCURS_CHECK_INFINITE_TYPE, UNEXPECTED_TYPE_FOR_EXPRESSION))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic)
import Diagnostic.Position (pointRange)
import qualified SyntaxGen.AbsStella as AST
import Type.Constraint
import Type.Core (Type (Type), fv)
import Type.Substitution (Substitution)
import qualified Type.Substitution as Substitution

unify :: Constraints -> Either Diagnostic Substitution
unify [] =
  return Substitution.empty
unify (Eq _ s t : cs) | s == t = unify cs
unify (Eq p (Type (AST.TypeVar () (AST.StellaIdent x))) t : cs)
  | not $ x `Set.member` fv t = do
      subst <- unify (fmap (Substitution.applyConstraint $ Substitution.singleton x t) cs)
      return $ Substitution.insert x t subst
  | otherwise = do
      let message = "mu " ++ x ++ ". " ++ show t
      Left $ diagnostic Error OCCURS_CHECK_INFINITE_TYPE (pointRange p) message
unify (Eq p s (Type (AST.TypeVar () (AST.StellaIdent x))) : cs)
  | not $ x `Set.member` fv s = do
      subst <- unify (fmap (Substitution.applyConstraint $ Substitution.singleton x s) cs)
      return $ Substitution.insert x s subst
  | otherwise = do
      let message = "mu " ++ x ++ ". " ++ show s
      Left $ diagnostic Error OCCURS_CHECK_INFINITE_TYPE (pointRange p) message
unify (Eq p (Type (AST.TypeFun () ss s)) (Type (AST.TypeFun () ts t)) : cs) = do
  let ss' = fmap Type $ s : ss
      ts' = fmap Type $ t : ts
      cs' = uncurry (Eq p) <$> zip ss' ts'
  unify $ cs' ++ cs
unify (Eq p s t : _) = do
  let message = "can't unify " ++ show s ++ " and " ++ show t
  Left $ diagnostic Error UNEXPECTED_TYPE_FOR_EXPRESSION (pointRange p) message
