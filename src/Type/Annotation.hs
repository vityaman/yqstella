{-# LANGUAGE TupleSections #-}

module Type.Annotation (annotateType) where

import Annotation (annotation)
import Control.Monad (unless, when)
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (find)
import Diagnostic (Diagnostic (Diagnostic), Diagnostics, Severity (Error), notImplemented)
import Position (Position, pointRange)
import qualified Syntax.AbsStella as AST
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type)
import qualified Type.Core as Type

type TypeAnnotationEnv a = WriterT Diagnostics (State Context) a

withStateTAE :: (Context -> Context) -> TypeAnnotationEnv a -> TypeAnnotationEnv a
withStateTAE f m = do
  old <- get
  modify f
  result <- m
  put old
  return result

class TypeAnnotatable f where
  annotateType :: f Position -> TypeAnnotationEnv (f (Position, Maybe Type))

instance TypeAnnotatable AST.Program' where
  annotateType (AST.AProgram p languagedecl extensions decls) = do
    let languagedecl' = fmap (,Nothing) languagedecl
        extensions' = fmap (fmap (,Nothing)) extensions
    decls' <- mapM annotateType decls

    let main = find isMain decls'
        type_ = main >>= declFunType

    return (AST.AProgram (p, type_) languagedecl' extensions' decls')
    where
      isMain (AST.DeclFun _ _ (AST.StellaIdent name) _ _ _ _ _) | name == "main" = True
      isMain _ = False

      declFunType (AST.DeclFun (_, type_') _ _ _ _ _ _ _) = type_'
      declFunType _ = error "Main declaration must be a function"

instance TypeAnnotatable AST.Decl' where
  annotateType (AST.DeclFun p annotations stellaident paramdecls returntype throwtype decls expr) = do
    let annotations' = fmap (fmap (,Nothing)) annotations
        paramdecls' = fmap (fmap (,Nothing)) paramdecls
        returntype' = fmap (,Nothing) returntype
        throwtype' = fmap (,Nothing) throwtype
        decls' = fmap (fmap (,Nothing)) decls

    unless (null annotations) $ tell [notImplemented p "DeclFun annotations"]

    unless (null decls) $ tell [notImplemented p "NestedFunctionDeclarations"]

    _ <- case throwtype of
      (AST.NoThrowType _) -> pure ()
      (AST.SomeThrowType _ _) -> tell [notImplemented p "DeclFun ThrowType"]

    let toPair (AST.AParamDecl _ (AST.StellaIdent key) t) = (key, Type.fromAST t)
        withDecl paramdecl = let (key, t) = toPair paramdecl in Context.withTyped key t
        extend paramdecls'' context = foldr withDecl context paramdecls''

    expr' <- withStateTAE (extend paramdecls) (annotateType expr)
    let (actualPosition, actualType) = annotation expr'
        argTypes = fmap (snd . toPair) paramdecls'
        type' = fmap (Type.fn argTypes) actualType

    _ <- case (returntype', actualType) of
      (AST.SomeReturnType _ expectedType, Just actualType')
        | expectedType' /= actualType' ->
            tell [mismatch actualPosition expectedType' actualType']
        where
          expectedType' = Type.fromAST expectedType
      _ -> return ()

    return (AST.DeclFun (p, type') annotations' stellaident paramdecls' returntype' throwtype' decls' expr')
  annotateType f@(AST.DeclFunGeneric {}) = do
    return $ fmap (,Nothing) f
  annotateType f@(AST.DeclTypeAlias {}) = do
    return $ fmap (,Nothing) f
  annotateType f@(AST.DeclExceptionType {}) = do
    return $ fmap (,Nothing) f
  annotateType f@(AST.DeclExceptionVariant {}) = do
    return $ fmap (,Nothing) f

instance TypeAnnotatable AST.LocalDecl' where
  annotateType (AST.ALocalDecl p decl) = do
    decl' <- annotateType decl
    let type' = snd $ annotation decl'
    return (AST.ALocalDecl (p, type') decl')

instance TypeAnnotatable AST.Expr' where
  annotateType x@(AST.Sequence p _ _) = do
    tell [notImplemented p "Sequence"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Assign {}) = do
    tell [notImplemented (annotation x) "Assign"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.If {}) = do
    tell [notImplemented (annotation x) "If"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Let {}) = do
    tell [notImplemented (annotation x) "Let"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.LetRec {}) = do
    tell [notImplemented (annotation x) "LetRec"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TypeAbstraction {}) = do
    tell [notImplemented (annotation x) "TypeAbstraction"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.LessThan {}) = do
    tell [notImplemented (annotation x) "LessThan"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.LessThanOrEqual {}) = do
    tell [notImplemented (annotation x) "LessThanOrEqual"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.GreaterThan {}) = do
    tell [notImplemented (annotation x) "GreaterThan"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.GreaterThanOrEqual {}) = do
    tell [notImplemented (annotation x) "AST"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Equal {}) = do
    tell [notImplemented (annotation x) "Equal"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.NotEqual {}) = do
    tell [notImplemented (annotation x) "NotEqual"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TypeAsc {}) = do
    tell [notImplemented (annotation x) "TypeAsc"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TypeCast {}) = do
    tell [notImplemented (annotation x) "TypeCast"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Abstraction {}) = do
    tell [notImplemented (annotation x) "Abstraction"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Variant {}) = do
    tell [notImplemented (annotation x) "Variant"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Match {}) = do
    tell [notImplemented (annotation x) "Match"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.List {}) = do
    tell [notImplemented (annotation x) "List"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Add {}) = do
    tell [notImplemented (annotation x) "Add"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Subtract {}) = do
    tell [notImplemented (annotation x) "Subtract"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.LogicOr {}) = do
    tell [notImplemented (annotation x) "LogicOr"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Multiply {}) = do
    tell [notImplemented (annotation x) "Multiply"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Divide {}) = do
    tell [notImplemented (annotation x) "Divide"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.LogicAnd {}) = do
    tell [notImplemented (annotation x) "LogicAnd"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Ref {}) = do
    tell [notImplemented (annotation x) "Ref"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Deref {}) = do
    tell [notImplemented (annotation x) "Deref"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Application {}) = do
    tell [notImplemented (annotation x) "Application"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TypeApplication {}) = do
    tell [notImplemented (annotation x) "TypeApplication"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.DotRecord {}) = do
    tell [notImplemented (annotation x) "DotRecord"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.DotTuple {}) = do
    tell [notImplemented (annotation x) "DotTuple"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Tuple {}) = do
    tell [notImplemented (annotation x) "Tuple"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Record {}) = do
    tell [notImplemented (annotation x) "Record"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConsList {}) = do
    tell [notImplemented (annotation x) "ConsList"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Head {}) = do
    tell [notImplemented (annotation x) "Head"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.IsEmpty {}) = do
    tell [notImplemented (annotation x) "IsEmpty"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Tail {}) = do
    tell [notImplemented (annotation x) "Tail"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Panic {}) = do
    tell [notImplemented (annotation x) "Panic"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Throw {}) = do
    tell [notImplemented (annotation x) "Throw"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TryCatch {}) = do
    tell [notImplemented (annotation x) "TryCatch"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TryWith {}) = do
    tell [notImplemented (annotation x) "TryWith"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.TryCastAs {}) = do
    tell [notImplemented (annotation x) "TryCastAs"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Inl {}) = do
    tell [notImplemented (annotation x) "Inl"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Inr {}) = do
    tell [notImplemented (annotation x) "Inr"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Succ {}) = do
    tell [notImplemented (annotation x) "Succ"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.LogicNot {}) = do
    tell [notImplemented (annotation x) "LogicNot"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Pred {}) = do
    tell [notImplemented (annotation x) "Pred"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.IsZero {}) = do
    tell [notImplemented (annotation x) "IsZero"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Fix {}) = do
    tell [notImplemented (annotation x) "Fix"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.NatRec {}) = do
    tell [notImplemented (annotation x) "NatRec"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Fold {}) = do
    tell [notImplemented (annotation x) "Fold"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Unfold {}) = do
    tell [notImplemented (annotation x) "Unfold"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConstTrue {}) = do
    tell [notImplemented (annotation x) "ConstTrue"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConstFalse {}) = do
    tell [notImplemented (annotation x) "ConstFalse"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConstUnit {}) = do
    tell [notImplemented (annotation x) "ConstUnit"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConstInt {}) = do
    tell [notImplemented (annotation x) "ConstInt"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConstMemory {}) = do
    tell [notImplemented (annotation x) "ConstMemory"]
    return $ fmap (,Nothing) x
  annotateType (AST.Var p stellaident@(AST.StellaIdent name)) = do
    context <- get
    let type' = Context.typeOf name context
    when (null type') $ tell [Context.unknownName p name]
    return $ AST.Var (p, type') stellaident

mismatch :: Position -> Type -> Type -> Diagnostic
mismatch position expected actual =
  let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
   in Diagnostic Error (pointRange position) message
