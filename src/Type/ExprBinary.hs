module Type.ExprBinary (annotateTT2T, annotateTT2B) where

import Annotation (Annotated, annotation)
import Control.Applicative (Alternative ((<|>)))
import Diagnostic.Position (Position)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf)
import Type.Expectation (liftType)

annotateTT2T ::
  (Annotated f, Annotated g) =>
  TypeAnnotator f ->
  TypeAnnotator g ->
  Maybe Type ->
  f Position ->
  g Position ->
  TypeAnnotationEnv (Maybe Type, f (Position, Maybe Type), g (Position, Maybe Type))
annotateTT2T annL annR t lhs rhs = do
  lhs' <- annL t lhs
  let lhs't = typeOf lhs'

  rhs' <- annR (t <|> lhs't) rhs
  let rhs't = typeOf rhs'

  return (lhs't <|> rhs't, lhs', rhs')

annotateTT2B ::
  (Annotated f, Annotated g) =>
  TypeAnnotator f ->
  TypeAnnotator g ->
  Maybe Type ->
  f Position ->
  g Position ->
  TypeAnnotationEnv (Maybe Type, f (Position, Maybe Type), g (Position, Maybe Type))
annotateTT2B annL annR t lhs rhs = do
  let p = annotation rhs

  t' <- liftType p AST.TypeBool t

  lhs' <- annL Nothing lhs
  let lhs't = typeOf lhs'

  rhs' <- annR lhs't rhs

  return (Just t', lhs', rhs')
