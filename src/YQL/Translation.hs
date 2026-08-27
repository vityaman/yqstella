module YQL.Translation (toYQL) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (runWriter)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as Set
import Diagnostic.Core (Diagnostic, notImplemented)
import Diagnostic.Position (Position, unknown)
import Extension.Activation (enabledExtensions)
import Extension.Core (Extension (..), extensionName)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Env (typeOf)
import YQL.AST (Node (..))

class YQLTranslatable f where
  toYQL :: f (Position, Maybe Type) -> Either Diagnostic Node

identNode :: AST.StellaIdent -> Node
identNode (AST.StellaIdent name) = A name

functionToYQL ::
  (YQLTranslatable param, YQLTranslatable expr) =>
  String ->
  [AST.StellaIdent] ->
  [param (Position, Maybe Type)] ->
  [AST.Decl' (Position, Maybe Type)] ->
  expr (Position, Maybe Type) ->
  Either Diagnostic Node
functionToYQL name parameters paramdecls decls expr = do
  paramdecls' <- mapM toYQL paramdecls
  decls' <- mapM toYQL (orderDecls decls)
  expr' <- toYQL expr

  let body =
        if null decls'
          then expr'
          else Y [A "block", Q $ Y $ decls' ++ [Y [A "return", expr']]]
      function = Y [A "lambda", Q (Y paramdecls'), body]
      genericFunction =
        if null parameters
          then function
          else Y [A "lambda", Q $ Y (fmap identNode parameters), function]

  return $ Y [A "let", A name, genericFunction]

instance YQLTranslatable AST.Program' where
  toYQL f@(AST.AProgram _ _ _ decls) = do
    (paramdecls, resultType) <- case mainSignatures decls of
      [x] -> Right x
      xs -> Left $ unsupported f $ "expected an only main, got " ++ show (length xs)

    let (extensions', _) = runWriter $ enabledExtensions $ fmap fst f
    () <- checkExtensions $ Set.toList extensions'

    topdecls' <- mapM toYQL (orderDecls decls)
    paramdecls' <- concat <$> mapM bindParameter paramdecls
    mainargs' <- mapM toMainArg paramdecls
    result' <- materializeCallable resultType (Y $ [A "Apply", A "main"] ++ mainargs')

    let rows = Y [A "AsList", Y [A "AsStruct", Q $ Y [Q $ A "result", A "result"]]]

    let main' =
          prelude "pure"
            ++ topdecls'
            ++ paramdecls'
            ++ [Y [A "let", A "result", result']]
            ++ [Y [A "let", A "world", Y [A "Apply", A "print", A "world", rows]]]
            ++ [Y [A "return", A "world"]]

    return $ Y main'
    where
      mainSignatures = concatMap mainSignature

      mainSignature (AST.DeclFun (_, Just (Type (AST.TypeFun _ _ result))) _ (AST.StellaIdent "main") paramdecls _ _ _ _) =
        [(paramdecls, Type result)]
      mainSignature _ = []

      declare :: AST.ParamDecl' (Position, Maybe Type) -> Either Diagnostic Node
      declare (AST.AParamDecl (_, t'') (AST.StellaIdent name') _) = do
        let t''' = fmap (const (unknown, Nothing)) . Type.toAST <$> t''
        t' <- toYQL $ fromMaybe (error "expected a paramdecl type") t'''
        return $ Y [A "declare", A $ "__" ++ name' ++ "__", t']

      bindParameter (AST.AParamDecl (_, Just type_@(Type (AST.TypeFun {}))) (AST.StellaIdent name') _) = do
        value <- defaultValueYQL type_
        return [Y [A "let", A $ "__" ++ name' ++ "__", value]]
      bindParameter param = pure <$> declare param

      toMainArg (AST.AParamDecl _ (AST.StellaIdent name') _) = do
        Right $ A $ "__" ++ name' ++ "__"

      materializeCallable (Type (AST.TypeFun _ arguments result)) callable = do
        arguments' <- mapM (defaultValueYQL . Type) arguments
        materializeCallable (Type result) (Y $ [A "Apply", callable] ++ arguments')
      materializeCallable _ value = Right value

orderDecls :: [AST.Decl' a] -> [AST.Decl' a]
orderDecls = go []
  where
    go ordered [] = ordered
    go ordered remaining =
      let remainingNames = Set.fromList $ mapMaybe declName remaining
          isReady decl =
            let dependencies = maybe id Set.delete (declName decl) (declVariables decl)
             in Set.null $ dependencies `Set.intersection` remainingNames
       in case break isReady remaining of
            (_, []) -> ordered ++ remaining
            (before, ready : after) -> go (ordered ++ [ready]) (before ++ after)

declName :: AST.Decl' a -> Maybe String
declName (AST.DeclFun _ _ (AST.StellaIdent name) _ _ _ _ _) = Just name
declName (AST.DeclFunGeneric _ _ (AST.StellaIdent name) _ _ _ _ _ _) = Just name
declName _ = Nothing

declVariables :: AST.Decl' a -> Set.Set String
declVariables (AST.DeclFun _ _ _ parameters _ _ decls expr) =
  functionVariables parameters decls expr
declVariables (AST.DeclFunGeneric _ _ _ _ parameters _ _ decls expr) =
  functionVariables parameters decls expr
declVariables _ = Set.empty

functionVariables :: [AST.ParamDecl' a] -> [AST.Decl' a] -> AST.Expr' a -> Set.Set String
functionVariables parameters decls expr =
  let bound = Set.fromList $ fmap paramName parameters ++ mapMaybe declName decls
      used = Set.unions $ exprVariables expr : fmap declVariables decls
   in used `Set.difference` bound
  where
    paramName (AST.AParamDecl _ (AST.StellaIdent name) _) = name

exprVariables :: AST.Expr' a -> Set.Set String
exprVariables expression = case expression of
  AST.Sequence _ lhs rhs -> both lhs rhs
  AST.Assign _ lhs rhs -> both lhs rhs
  AST.If _ condition thenBranch elseBranch -> Set.unions $ fmap exprVariables [condition, thenBranch, elseBranch]
  AST.Let _ bindings body -> bindingVariables bindings `Set.union` exprVariables body
  AST.LetRec _ bindings body -> bindingVariables bindings `Set.union` exprVariables body
  AST.TypeAbstraction _ _ expr -> exprVariables expr
  AST.LessThan _ lhs rhs -> both lhs rhs
  AST.LessThanOrEqual _ lhs rhs -> both lhs rhs
  AST.GreaterThan _ lhs rhs -> both lhs rhs
  AST.GreaterThanOrEqual _ lhs rhs -> both lhs rhs
  AST.Equal _ lhs rhs -> both lhs rhs
  AST.NotEqual _ lhs rhs -> both lhs rhs
  AST.TypeAsc _ expr _ -> exprVariables expr
  AST.TypeCast _ expr _ -> exprVariables expr
  AST.Abstraction _ parameters expr ->
    exprVariables expr `Set.difference` Set.fromList (fmap paramName parameters)
  AST.Variant _ _ data' -> exprDataVariables data'
  AST.Match _ expr cases -> exprVariables expr `Set.union` Set.unions (fmap caseVariables cases)
  AST.List _ exprs -> Set.unions $ fmap exprVariables exprs
  AST.Add _ lhs rhs -> both lhs rhs
  AST.Subtract _ lhs rhs -> both lhs rhs
  AST.LogicOr _ lhs rhs -> both lhs rhs
  AST.Multiply _ lhs rhs -> both lhs rhs
  AST.Divide _ lhs rhs -> both lhs rhs
  AST.LogicAnd _ lhs rhs -> both lhs rhs
  AST.Ref _ expr -> exprVariables expr
  AST.Deref _ expr -> exprVariables expr
  AST.Application _ function arguments -> exprVariables function `Set.union` Set.unions (fmap exprVariables arguments)
  AST.TypeApplication _ expr _ -> exprVariables expr
  AST.DotRecord _ expr _ -> exprVariables expr
  AST.DotTuple _ expr _ -> exprVariables expr
  AST.Tuple _ exprs -> Set.unions $ fmap exprVariables exprs
  AST.Record _ bindings -> Set.unions $ fmap recordBindingVariables bindings
  AST.ConsList _ head' tail' -> both head' tail'
  AST.Head _ expr -> exprVariables expr
  AST.IsEmpty _ expr -> exprVariables expr
  AST.Tail _ expr -> exprVariables expr
  AST.Panic _ -> Set.empty
  AST.Throw _ expr -> exprVariables expr
  AST.TryCatch _ expr _ fallback -> both expr fallback
  AST.TryWith _ expr fallback -> both expr fallback
  AST.TryCastAs _ expr _ _ success fallback -> Set.unions $ fmap exprVariables [expr, success, fallback]
  AST.Inl _ expr -> exprVariables expr
  AST.Inr _ expr -> exprVariables expr
  AST.Succ _ expr -> exprVariables expr
  AST.LogicNot _ expr -> exprVariables expr
  AST.Pred _ expr -> exprVariables expr
  AST.IsZero _ expr -> exprVariables expr
  AST.Fix _ expr -> exprVariables expr
  AST.NatRec _ n initial step -> Set.unions $ fmap exprVariables [n, initial, step]
  AST.Fold _ _ expr -> exprVariables expr
  AST.Unfold _ _ expr -> exprVariables expr
  AST.ConstTrue _ -> Set.empty
  AST.ConstFalse _ -> Set.empty
  AST.ConstUnit _ -> Set.empty
  AST.ConstInt _ _ -> Set.empty
  AST.ConstMemory _ _ -> Set.empty
  AST.Var _ (AST.StellaIdent name) -> Set.singleton name
  where
    both lhs rhs = exprVariables lhs `Set.union` exprVariables rhs
    bindingVariables = Set.unions . fmap (\(AST.APatternBinding _ _ expr) -> exprVariables expr)
    recordBindingVariables (AST.ABinding _ _ expr) = exprVariables expr
    caseVariables (AST.AMatchCase _ _ expr) = exprVariables expr
    exprDataVariables (AST.NoExprData _) = Set.empty
    exprDataVariables (AST.SomeExprData _ expr) = exprVariables expr
    paramName (AST.AParamDecl _ (AST.StellaIdent name) _) = name

instance YQLTranslatable AST.Decl' where
  toYQL (AST.DeclFun _ _ (AST.StellaIdent name) paramdecls _ (AST.NoThrowType _) decls expr) =
    functionToYQL name [] paramdecls decls expr
  toYQL (AST.DeclFunGeneric _ _ (AST.StellaIdent name) parameters paramdecls _ (AST.NoThrowType _) decls expr) =
    functionToYQL name parameters paramdecls decls expr
  toYQL (AST.DeclTypeAlias _ (AST.StellaIdent name) _) = do
    return $ Y [A "let", A name, Y [A "Void"]]
  toYQL x = Left $ unsupported x "AST.Decl'"

instance YQLTranslatable AST.ParamDecl' where
  toYQL (AST.AParamDecl _ (AST.StellaIdent name) _) = do
    return $ A name

instance YQLTranslatable AST.Binding' where
  toYQL (AST.ABinding _ (AST.StellaIdent name) expr) = do
    expr' <- toYQL expr
    return $ Q $ Y [Q $ A name, expr']

instance YQLTranslatable AST.Expr' where
  toYQL (AST.TypeAbstraction _ parameters expr) = do
    expr' <- toYQL expr
    return $ Y [A "lambda", Q $ Y (fmap identNode parameters), expr']
  toYQL (AST.TypeApplication _ expr types) = do
    expr' <- toYQL expr
    types' <- mapM toYQL types
    return $ Y ([A "Apply", expr'] ++ types')
  toYQL (AST.LessThan _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "<", lhs', rhs']
  toYQL (AST.LessThanOrEqual _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "<=", lhs', rhs']
  toYQL (AST.GreaterThan _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A ">", lhs', rhs']
  toYQL (AST.GreaterThanOrEqual _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A ">=", lhs', rhs']
  toYQL (AST.Equal _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "==", lhs', rhs']
  toYQL (AST.NotEqual _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "!=", lhs', rhs']
  toYQL (AST.TypeAsc _ expr _) =
    toYQL expr
  toYQL (AST.Add (_, _) lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "+MayWarn", lhs', rhs']
  toYQL (AST.Subtract (_, t) lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ case t of
      (Just (Type (AST.TypeNat ()))) ->
        Y
          [ A "If",
            Y [A "<", lhs', rhs'],
            Y [A "Uint64", Q $ A "0"],
            Y [A "-MayWarn", lhs', rhs']
          ]
      _ ->
        Y [A "-MayWarn", lhs', rhs']
  toYQL (AST.Multiply _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "*MayWarn", lhs', rhs']
  toYQL (AST.Divide _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "Unwrap", Y [A "/MayWarn", lhs', rhs']]
  toYQL (AST.If _ condition thenB elseB) = do
    condition' <- toYQL condition
    thenB' <- toYQL thenB
    elseB' <- toYQL elseB
    return $ Y [A "If", condition', thenB', elseB']
  toYQL (AST.Let _ [AST.APatternBinding _ (AST.PatternVar _ (AST.StellaIdent name)) expr] inExpr) = do
    expr' <- toYQL expr
    inExpr' <- toYQL inExpr
    return $ Y [A "block", Q $ Y [Y [A "let", A name, expr'], Y [A "return", inExpr']]]
  toYQL (AST.Let _ bindings@(_ : _ : _) inExpr)
    | Just names <- traverse bindingName bindings = do
        bindings' <- mapM bindingExpr bindings
        inExpr' <- toYQL inExpr
        let lambda = Y [A "lambda", Q $ Y $ fmap A names, inExpr']
        return $ Y $ [A "Apply", lambda] ++ bindings'
    where
      bindingName (AST.APatternBinding _ (AST.PatternVar _ (AST.StellaIdent name)) _) = Just name
      bindingName _ = Nothing

      bindingExpr (AST.APatternBinding _ _ expr) = toYQL expr
  toYQL (AST.Let (p, t) [AST.APatternBinding (p', t') pattern' expr] inExpr) = do
    toYQL (AST.Match (p, t) expr [AST.AMatchCase (p', t') pattern' inExpr])
  toYQL (AST.Abstraction _ paramdecls expr) = do
    paramdecls' <- mapM toYQL paramdecls
    expr' <- toYQL expr
    return $ Y [A "lambda", Q (Y paramdecls'), expr']
  toYQL (AST.Variant (_, Just (Type t)) (AST.StellaIdent tag) (AST.SomeExprData _ expr)) = do
    t' <- toYQL (fmap (const (unknown, Nothing)) t)
    expr' <- toYQL expr
    return $ Y [A "Variant", expr', Q $ A tag, t']
  toYQL (AST.Variant p tag (AST.NoExprData p')) =
    toYQL (AST.Variant p tag (AST.SomeExprData p' (AST.ConstUnit p')))
  toYQL (AST.Match _ expr cases) = do
    let arg = "yqstellamatchexpr"
        brprefix = "yqstellamatchbr"

    expr' <- toYQL expr
    cases' <- mapM toYQL cases

    let brnames = [brprefix ++ show i | i <- [0 .. length cases - 1]]

        args = Y [A "let", A arg, expr']
        branches = [Y [A "let", A name, Y [A "Apply", case', A arg]] | (name, case') <- zip brnames cases']
        switch = Y [A "return", Y [A "Unwrap", Y $ A "Coalesce" : fmap A brnames]]

        body = [args] ++ branches ++ [switch]

    return $ Y [A "block", Q $ Y body]
  toYQL (AST.List (_, Just (Type (AST.TypeList () t))) []) = do
    t' <- toYQL $ fmap (const (unknown, Nothing)) t
    return $ Y [A "ToList", Y [A "Nothing", Y [A "OptionalType", t']]]
  toYQL (AST.List _ (x : xs)) = do
    exprs' <- mapM toYQL (x : xs)
    return $ Y $ A "AsList" : exprs'
  toYQL (AST.Application _ f xs) = do
    f' <- toYQL f
    xs' <- mapM toYQL xs
    return $ Y $ [A "Apply", f'] ++ xs'
  toYQL (AST.DotRecord _ expr (AST.StellaIdent field)) = do
    expr' <- toYQL expr
    return $ Y [A "Member", expr', Q (A field)]
  toYQL (AST.DotTuple _ expr index) = do
    expr' <- toYQL expr
    return $ Y [A "Nth", expr', Q $ A $ show (index - 1)]
  toYQL (AST.Tuple _ exprs) = do
    exprs' <- mapM toYQL exprs
    return $ Q $ Y exprs'
  toYQL (AST.Record _ bindings) = do
    bindings' <- mapM toYQL bindings
    return $ Y $ A "AsStruct" : bindings'
  toYQL (AST.ConsList _ head'' tail'') = do
    head' <- toYQL head''
    tail' <- toYQL tail''
    return $ Y [A "Prepend", head', tail']
  toYQL (AST.Head _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "Unwrap", Y [A "ToOptional", expr']]
  toYQL (AST.IsEmpty _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "Not", Y [A "HasItems", expr']]
  toYQL (AST.Tail _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "Skip", expr', Y [A "Uint64", Q $ A "1"]]
  toYQL (AST.Panic (p, Just t)) = do
    value <- defaultValueYQL t
    false <- toYQL (AST.ConstFalse (unknown, Just $ Type.fromAST' AST.TypeBool))
    let message = Y [A "String", Q $ A $ "\"" ++ "panic at :" ++ show p ++ "\""]
    return $ Y [A "Ensure", value, false, message]
  toYQL (AST.Inl (_, Just (Type t)) expr) = do
    t' <- toYQL $ fmap (const (unknown, Nothing)) t
    expr' <- toYQL expr
    return $ Y [A "Variant", expr', Q $ A "inl", t']
  toYQL (AST.Inr (_, Just (Type t)) expr) = do
    t' <- toYQL $ fmap (const (unknown, Nothing)) t
    expr' <- toYQL expr
    return $ Y [A "Variant", expr', Q $ A "inr", t']
  toYQL (AST.Succ _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "+", expr', Y [A "Uint64", Q (A "1")]]
  toYQL (AST.IsZero _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "==", expr', Y [A "Uint64", Q (A "0")]]
  toYQL (AST.ConstTrue _) = do
    return $ Y [A "Bool", Q (A "true")]
  toYQL (AST.ConstFalse _) = do
    return $ Y [A "Bool", Q (A "false")]
  toYQL (AST.ConstInt (_, Just (Type (AST.TypeNat _))) n) =
    return $ Y [A "Uint64", Q (A $ show n)]
  toYQL (AST.ConstUnit _) = do
    return $ Y [A "Void"]
  toYQL (AST.LogicOr _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "Or", lhs', rhs']
  toYQL (AST.LogicAnd _ lhs rhs) = do
    lhs' <- toYQL lhs
    rhs' <- toYQL rhs
    return $ Y [A "And", lhs', rhs']
  toYQL (AST.LogicNot _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "Not", expr']
  toYQL (AST.Pred _ expr) = do
    expr' <- toYQL expr
    return $ Y [A "-", expr', Y [A "Uint64", Q (A "1")]]
  toYQL (AST.Var _ (AST.StellaIdent name)) = do
    return $ A name
  toYQL x = Left $ unsupported x "AST.Expr'"

instance YQLTranslatable AST.Type' where
  toYQL (AST.TypeForAll {}) = Right $ Y [A "VoidType"]
  toYQL (AST.TypeFun _ argts returnts) = do
    argts' <- mapM toYQL argts
    returnt' <- toYQL returnts
    Right $ Y [A "CallableType", Q $ Y [], Q $ Y [returnt'], Q $ Y argts']
  toYQL (AST.TypeSum _ inl inr) = do
    inl' <- toYQL inl
    inr' <- toYQL inr
    Right $
      Y
        [ A "VariantType",
          Y
            [ A "StructType",
              Q $ Y [Q $ A "inl", inl'],
              Q $ Y [Q $ A "inr", inr']
            ]
        ]
  toYQL (AST.TypeTuple _ ts) = do
    ts' <- mapM toYQL ts
    Right $ Y $ A "TupleType" : ts'
  toYQL (AST.TypeRecord _ fields) = do
    fields' <- mapM toYQL' fields
    Right $ Y $ A "StructType" : fields'
    where
      toYQL' (AST.ARecordFieldType _ (AST.StellaIdent name) t) = do
        t' <- toYQL t
        Right $ Q $ Y [Q $ A name, t']
  toYQL (AST.TypeVariant _ fields) = do
    fields' <- mapM toYQL' fields
    Right $ Y [A "VariantType", Y $ A "StructType" : fields']
    where
      toYQL' (AST.AVariantFieldType _ (AST.StellaIdent name) (AST.NoTyping _)) = do
        Right $ Q $ Y [Q $ A name, Y [A "VoidType"]]
      toYQL' (AST.AVariantFieldType _ (AST.StellaIdent name) (AST.SomeTyping _ t)) = do
        t' <- toYQL t
        Right $ Q $ Y [Q $ A name, t']
  toYQL (AST.TypeList _ t) = do
    t' <- toYQL t
    Right $ Y [A "ListType", t']
  toYQL (AST.TypeBool _) = Right $ Y [A "DataType", Q $ A "Bool"]
  toYQL (AST.TypeNat _) = Right $ Y [A "DataType", Q $ A "Uint64"]
  toYQL (AST.TypeUnit _) = Right $ Y [A "VoidType"]
  toYQL (AST.TypeVar _ ident) = Right $ identNode ident
  toYQL x = Left $ unsupported x "AST.Type'"

instance YQLTranslatable AST.MatchCase' where
  toYQL (AST.AMatchCase _ pattern' expr) = do
    recipes' <- recipes pattern'

    t' <- case typeOf expr of
      (Just (Type t')) -> toYQL $ fmap (const (unknown, Nothing)) t'
      Nothing -> Left $ unsupported expr "expected Nothing type"

    expr' <- toYQL expr

    let arg = A "yqstellamatcharg"
        cond = A "yqstellamatchcond"

        maybes = [Y [A "let", A name, f arg] | (name, f) <- Map.toList recipes']
        conds = Y [A "let", cond, and' [Y [A "Exists", A name] | (name, _) <- Map.toList recipes']]
        unwraps = [Y [A "let", A name, Y [A "Unwrap", A name]] | (name, _) <- Map.toList recipes']
        switch = Y [A "return", Y [A "If", cond, Y [A "Just", expr'], Y [A "Nothing", Y [A "OptionalType", t']]]]

        body = maybes ++ [conds] ++ unwraps ++ [switch]

        and' [] = Y [A "Bool", Q $ A "true"]
        and' (x : xs) = Y [A "And", x, and' xs]

    return $ Y [A "lambda", Q $ Y [arg], Y [A "block", Q $ Y body]]

recipes :: AST.Pattern' (Position, Maybe Type) -> Either Diagnostic (Map String (Node -> Node))
recipes (AST.PatternVariant _ (AST.StellaIdent tag) (AST.SomePatternData _ pattern'')) = do
  recipes'' <- recipes pattern''
  let recipes' = fmap (\f x -> f $ Y [A "Guess", x, Q $ A tag]) recipes''
  return recipes'
recipes (AST.PatternVariant p (AST.StellaIdent tag) (AST.NoPatternData p')) = do
  let pattern'' = AST.PatternUnit p'
  recipes (AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' pattern''))
recipes (AST.PatternInl _ pattern'') = do
  recipes'' <- recipes pattern''
  let recipes' = fmap (\f x -> f $ Y [A "Guess", x, Q $ A "inl"]) recipes''
  return recipes'
recipes (AST.PatternInr _ pattern'') = do
  recipes'' <- recipes pattern''
  let recipes' = fmap (\f x -> f $ Y [A "Guess", x, Q $ A "inr"]) recipes''
  return recipes'
recipes (AST.PatternTuple _ patterns'') = do
  recipes'' <- zip [0 ..] <$> mapM recipes patterns''

  let recipesF :: Integer -> (Node -> Node) -> Node -> Node
      recipesF i f x = f $ Y [A "Nth", x, Q $ A $ show i]

      recipesM :: Integer -> Map String (Node -> Node) -> Map String (Node -> Node)
      recipesM i = fmap (recipesF i)

  return $ Map.unions $ fmap (uncurry recipesM) recipes''
recipes (AST.PatternRecord _ patterns'') = do
  let recipesP (AST.ALabelledPattern _ (AST.StellaIdent name) pattern') = do
        recipes' <- recipes pattern'
        return (name, recipes')

  recipes'' <- mapM recipesP patterns''

  let recipesF :: String -> (Node -> Node) -> Node -> Node
      recipesF i f x = f $ Y [A "Member", x, Q $ A $ show i]

      recipesM :: String -> Map String (Node -> Node) -> Map String (Node -> Node)
      recipesM i = fmap (recipesF i)

  return $ Map.unions $ fmap (uncurry recipesM) recipes''
recipes (AST.PatternList (p, _) []) = do
  let core = Y [A "OptionalIf", Y [A "Not", Y [A "HasItems", A "x"]], Y [A "EmptyList"]]
      f x = mapcoerce' x $ Y [A "lambda", Q $ Y [A "x"], core]
      name = "yqstellamatchnil:" ++ show p
  return $ Map.singleton name f
recipes (AST.PatternList p (x : xs)) = do
  recipes (AST.PatternCons p x (AST.PatternList p xs))
recipes (AST.PatternCons (p, _) head' tail') = do
  one <- toYQL (AST.ConstInt (p, Just $ Type.fromAST' AST.TypeNat) 1)

  let headF :: (Node -> Node) -> (Node -> Node)
      headF f x = f $ Y [A "ToOptional", x]

      tailF :: (Node -> Node) -> (Node -> Node)
      tailF f x = f $ Y [A "Skip", x, one]

  head'' <- recipes head'
  tail'' <- recipes tail'
  return $ Map.union (fmap headF head'') (fmap tailF tail'')
recipes (AST.PatternFalse (p, t)) = do
  false <- toYQL (AST.ConstFalse (p, t))
  let f x = Y [A "OptionalIf", Y [A "Not", x], false]
  let name = "yqstellamatchfalse:" ++ show p
  return $ Map.singleton name f
recipes (AST.PatternTrue (p, t)) = do
  true <- toYQL (AST.ConstTrue (p, t))
  let f x = Y [A "OptionalIf", x, true]
  let name = "yqstellamatchtrue:" ++ show p
  return $ Map.singleton name f
recipes (AST.PatternUnit (p, _)) = do
  let name = "yqstellamatchunit:" ++ show p
  return $ Map.singleton name id
recipes (AST.PatternInt (p, t) n) = do
  int <- toYQL (AST.ConstInt (p, t) n)
  let core = Y [A "OptionalIf", Y [A "==", A "x", int], int]
      f x = mapcoerce' x $ Y [A "lambda", Q $ Y [A "x"], core]
      name = "yqstellamatchint:" ++ show p ++ ":" ++ show n
  return $ Map.singleton name f
recipes (AST.PatternSucc (p, t) pattern'') = do
  recipes'' <- recipes pattern''
  zero <- toYQL (AST.ConstInt (p, t) 0)
  one <- toYQL (AST.ConstInt (p, t) 1)
  let core = Y [A "OptionalIf", Y [A "!=", A "x", zero], Y [A "-MayWarn", A "x", one]]
      wrap f x = f $ mapcoerce' x $ Y [A "lambda", Q $ Y [A "x"], core]
      recipes' = fmap wrap recipes''
  return recipes'
recipes (AST.PatternVar _ (AST.StellaIdent name)) =
  Right $ Map.singleton name id
recipes x =
  Left $ unsupported x "AST.Pattern'"

defaultValueYQL :: Type -> Either Diagnostic Node
defaultValueYQL (Type type_) = case type_ of
  AST.TypeFun _ args result -> do
    result' <- defaultValueYQL (Type result)
    let parameters = [A $ "x" ++ show index | index <- [0 .. length args - 1]]
    return $ Y [A "lambda", Q $ Y parameters, result']
  AST.TypeForAll _ parameters body -> do
    body' <- defaultValueYQL (Type body)
    return $ Y [A "lambda", Q $ Y (fmap identNode parameters), body']
  AST.TypeSum _ left _ -> do
    typeNode <- toTypeNode type_
    value <- defaultValueYQL (Type left)
    return $ Y [A "Variant", value, Q $ A "inl", typeNode]
  AST.TypeTuple _ types -> Q . Y <$> mapM (defaultValueYQL . Type) types
  AST.TypeRecord _ fields -> do
    fields' <- mapM recordField fields
    return $ Y $ A "AsStruct" : fields'
  AST.TypeVariant _ (field : _) -> do
    typeNode <- toTypeNode type_
    case field of
      AST.AVariantFieldType _ (AST.StellaIdent label) (AST.NoTyping _) ->
        return $ Y [A "Variant", Y [A "Void"], Q $ A label, typeNode]
      AST.AVariantFieldType _ (AST.StellaIdent label) (AST.SomeTyping _ fieldType) -> do
        value <- defaultValueYQL (Type fieldType)
        return $ Y [A "Variant", value, Q $ A label, typeNode]
  AST.TypeList _ item -> do
    itemType <- toTypeNode item
    return $ Y [A "ToList", Y [A "Nothing", Y [A "OptionalType", itemType]]]
  AST.TypeBool _ -> return $ Y [A "Bool", Q $ A "false"]
  AST.TypeNat _ -> return $ Y [A "Uint64", Q $ A "0"]
  AST.TypeUnit _ -> return $ Y [A "Void"]
  _ -> do
    typeNode <- toTypeNode type_
    return $ Y [A "Unwrap", Y [A "Nothing", Y [A "OptionalType", typeNode]]]
  where
    recordField (AST.ARecordFieldType _ (AST.StellaIdent label) fieldType) = do
      value <- defaultValueYQL (Type fieldType)
      return $ Q $ Y [Q $ A label, value]

    toTypeNode = toYQL . fmap (const (unknown, Nothing))

checkExtensions :: [Extension] -> Either Diagnostic ()
checkExtensions extensions = case findUnsupported extensions of
  Nothing -> Right ()
  Just e -> Left $ unsupported' unknown ("Extension " ++ extensionName e)
  where
    isSupportedExtension :: Extension -> Bool
    isSupportedExtension StructuralPatterns = True
    isSupportedExtension TypeAliases = True
    isSupportedExtension UnitType = True
    isSupportedExtension Pairs = True
    isSupportedExtension Tuples = True
    isSupportedExtension Records = True
    isSupportedExtension SumTypes = True
    isSupportedExtension Lists = True
    isSupportedExtension Variants = True
    isSupportedExtension NullaryVariantLabels = True
    isSupportedExtension NullaryFunctions = True
    isSupportedExtension MultiparameterFunctions = True
    isSupportedExtension NestedFunctionDeclarations = True
    isSupportedExtension LetBindings = True
    isSupportedExtension LetManyBindings = True
    isSupportedExtension LetPatterns = True
    isSupportedExtension TypeAscriptions = True
    isSupportedExtension NaturalLiterals = True
    isSupportedExtension Predecessor = True
    isSupportedExtension ArithmeticOperators = True
    isSupportedExtension ComparisonOperations = True
    isSupportedExtension LogicalOperators = True
    isSupportedExtension Panic = True
    isSupportedExtension TypeReconstruction = True
    isSupportedExtension UniversalTypes = True
    isSupportedExtension _ = False

    findUnsupported :: [Extension] -> Maybe Extension
    findUnsupported = find (not . isSupportedExtension)

unsupported :: (Annotated f) => f (Position, Maybe Type) -> String -> Diagnostic
unsupported f = unsupported' (fst $ annotation f)

unsupported' :: Position -> String -> Diagnostic
unsupported' p reason =
  let message = "YQL translation unsupported: " ++ reason
   in notImplemented p message

mapcoerce' :: Node -> Node -> Node
mapcoerce' x f = Y [A "Apply", Y [A "lambda", Q $ Y [A "x", A "f"], body], x, f]
  where
    lambdaX x' = Y [A "lambda", Q $ Y [A "x"], x']
    apply = Y [A "Apply", A "f", A "x"]
    body =
      Y $
        [A "MatchType", A "x"]
          ++ [Q $ A "Optional", lambdaX $ Y [A "FlatMap", A "x", lambdaX apply]]
          ++ [{-             -} lambdaX {-                        -} apply]

prelude :: String -> [Node]
prelude provider =
  let print' = Y [A "let", A "print", Y [A "lambda", Q $ Y [A "world", A "rows"], Y [A "block", Q $ Y stmts]]]
        where
          stmts =
            [ Y [A "let", A "sink", Y [A "DataSink", Q $ A "result"]],
              Y [A "let", A "options", Q $ Y [Q $ Y [Q $ A "type"], Q $ Y [Q $ A "autoref"], Q $ Y [Q $ A "unordered"]]],
              Y [A "let", A "world", Y [A "ResFill!", A "world", A "sink", Y [A "Key"], A "rows", A "options", Q $ A provider]],
              Y [A "return", Y [A "Commit!", A "world", A "sink"]]
            ]
   in [print']
