{-# LANGUAGE TupleSections #-}

module Type.Annotation (annotateType) where

import Annotation (annotation)
import Control.Monad (unless, when)
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (find)
import Data.Maybe (fromMaybe)
import Diagnostic (Diagnostic (Diagnostic), Diagnostics, Severity (Error), notImplemented)
import Position (Position, pointRange)
import qualified Syntax.AbsStella as AST
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type (Type))
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

    expr' <- withStateTAE (ctxExtendByParamDecls paramdecls) (annotateType expr)
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
  annotateType (AST.If p condition thenB elseB) = do
    condition' <- annotateType condition
    thenB' <- annotateType thenB
    elseB' <- annotateType elseB

    condition'Type <-
      let (position', type') = annotation condition'
       in matchType position' AST.TypeBool type'

    let thenB'Type = snd $ annotation thenB'
        elseB'Type = snd $ annotation elseB'

    type' <- case (thenB'Type, elseB'Type) of
      (Just then', Just else') | then' == else' -> pure $ Just then'
      (Just then', Just else') -> do
        let message =
              "type mismatch: then branch type is "
                ++ show then'
                ++ ", else branch type is "
                ++ show else'
        tell [Diagnostic Error (pointRange p) message]
        return Nothing
      _ -> pure Nothing

    return $ AST.If (p, condition'Type >> type') condition' thenB' elseB'
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
  annotateType (AST.Abstraction p paramdecls expr) = do
    let paramdecls' = fmap (fmap (,Nothing)) paramdecls

    expr' <- withStateTAE (ctxExtendByParamDecls paramdecls) (annotateType expr)
    let (_, actualType) = annotation expr'
        argTypes = fmap (snd . toPair) paramdecls'
        type' = fmap (Type.fn argTypes) actualType

    return $ AST.Abstraction (p, type') paramdecls' expr'
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
  annotateType (AST.Application p expr exprs) = do
    expr' <- annotateType expr
    exprs' <- mapM annotateType exprs

    let (expr'Position, expr'Type) = annotation expr'

    type' <- case expr'Type of
      Just (Type (AST.TypeFun _ argTypes returnType)) -> do
        let expectedArgTypes = fmap Type argTypes

        tell $
          [ mismatch p' e a
            | ((p', Just a), e) <- zip (fmap annotation exprs') expectedArgTypes,
              a /= e
          ]

        return $ Just $ Type returnType
      Just actual -> do
        let unknown = Type.fromAST' AST.TypeAuto
            expectedArgTypes = fmap (fromMaybe unknown . snd . annotation) exprs'
            expected = Type.fn expectedArgTypes unknown

        let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
        tell [Diagnostic Error (pointRange expr'Position) message]

        return Nothing
      Nothing ->
        return Nothing

    return $ AST.Application (p, type') expr' exprs'
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
  annotateType (AST.Succ p expr) = do
    expr' <- annotateType expr

    type' <- matchType p AST.TypeNat (snd $ annotation expr')

    return $ AST.Succ (p, type') expr'
  annotateType x@(AST.LogicNot {}) = do
    tell [notImplemented (annotation x) "LogicNot"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.Pred {}) = do
    tell [notImplemented (annotation x) "Pred"]
    return $ fmap (,Nothing) x
  annotateType (AST.IsZero p expr) = do
    expr' <- annotateType expr

    expr'Type <- matchType p AST.TypeNat (snd $ annotation expr')
    let type' = fmap (const $ Type.fromAST' AST.TypeBool) expr'Type

    return $ AST.IsZero (p, type') expr'
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
  annotateType x@(AST.ConstTrue _) = do
    return $ fmap (,Just $ Type.fromAST' AST.TypeBool) x
  annotateType x@(AST.ConstFalse _) = do
    return $ fmap (,Just $ Type.fromAST' AST.TypeBool) x
  annotateType x@(AST.ConstUnit {}) = do
    tell [notImplemented (annotation x) "ConstUnit"]
    return $ fmap (,Nothing) x
  annotateType x@(AST.ConstInt p n) = do
    unless (n == 0) $ tell [notImplemented p "Non-Zero Integer"]
    let type' = if n == 0 then Just $ Type.fromAST' AST.TypeNat else Nothing
    return $ fmap (,type') x
  annotateType x@(AST.ConstMemory {}) = do
    tell [notImplemented (annotation x) "ConstMemory"]
    return $ fmap (,Nothing) x
  annotateType (AST.Var p stellaident@(AST.StellaIdent name)) = do
    context <- get
    let type' = Context.typeOf name context
    when (null type') $ tell [Context.unknownName p name]
    return $ AST.Var (p, type') stellaident

toPair :: AST.ParamDecl' a -> (String, Type)
toPair (AST.AParamDecl _ (AST.StellaIdent key) t) = (key, Type.fromAST t)

ctxExtendByParamDecls :: (Foldable f) => f (AST.ParamDecl' a) -> Context -> Context
ctxExtendByParamDecls paramdecls'' context = foldr withDecl context paramdecls''
  where
    withDecl paramdecl = let (key, t) = toPair paramdecl in Context.withTyped key t

matchType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv (Maybe Type)
matchType position expected = matchType' position expected'
  where
    expected' = Type.fromAST' expected

matchType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv (Maybe Type)
matchType' position expected actual =
  case actual of
    Just type' | type' == expected -> pure $ pure type'
    Just actual' -> tell [mismatch position expected actual'] >> pure Nothing
    Nothing -> pure Nothing

mismatch :: Position -> Type -> Type -> Diagnostic
mismatch position expected actual =
  let message = "type mismatch: expected " ++ show expected ++ ", got " ++ show actual
   in Diagnostic Error (pointRange position) message
