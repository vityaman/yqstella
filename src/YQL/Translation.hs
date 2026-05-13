module YQL.Translation (toYQL) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (runWriter)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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

instance YQLTranslatable AST.Program' where
  toYQL f@(AST.AProgram _ _ _ decls) = do
    paramdecls <- case mainparams decls of
      [x] -> Right x
      xs -> Left $ unsupported f $ "expected an only main, got " ++ show (length xs)

    let (extensions', _) = runWriter $ enabledExtensions $ fmap fst f
    () <- checkExtensions $ Set.toList extensions'

    topdecls' <- mapM toYQL decls
    paramdecls' <- mapM declare paramdecls
    mainargs' <- mapM toMainArg paramdecls

    let rows = Y [A "AsList", Y [A "AsStruct", Q $ Y [Q $ A "result", A "result"]]]

    let main' =
          prelude "pure"
            ++ topdecls'
            ++ paramdecls'
            ++ [Y [A "let", A "result", Y $ [A "Apply", A "main"] ++ mainargs']]
            ++ [Y [A "let", A "world", Y [A "Apply", A "print", A "world", rows]]]
            ++ [Y [A "return", A "world"]]

    return $ Y main'
    where
      mainparams = concatMap mainparams'

      mainparams' (AST.DeclFun _ _ (AST.StellaIdent "main") paramdecls _ _ _ _) = [paramdecls]
      mainparams' _ = []

      declare :: AST.ParamDecl' (Position, Maybe Type) -> Either Diagnostic Node
      declare (AST.AParamDecl (_, t'') (AST.StellaIdent name') _) = do
        let t''' = fmap (const (unknown, Nothing)) . Type.toAST <$> t''
        t' <- toYQL $ fromMaybe (error "expected a paramdecl type") t'''
        return $ Y [A "declare", A $ "__" ++ name' ++ "__", t']

      toMainArg (AST.AParamDecl _ (AST.StellaIdent name') _) = do
        Right $ A $ "__" ++ name' ++ "__"

instance YQLTranslatable AST.Decl' where
  toYQL (AST.DeclFun _ _ (AST.StellaIdent name) paramdecls _ (AST.NoThrowType _) decls expr) = do
    paramdecls' <- mapM toYQL paramdecls
    decls' <- mapM toYQL decls
    expr' <- toYQL expr

    let body =
          if null decls'
            then expr'
            else Y [A "block", Q $ Y $ decls' ++ [Y [A "return", expr']]]

    return $ Y [A "let", A name, Y [A "lambda", Q (Y paramdecls'), body]]
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
    value <- toYQL $ defaultValue t
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

defaultValue :: Type -> AST.Expr' (Position, Maybe Type)
defaultValue t@(Type (AST.TypeFun _ argts returnt)) =
  let type' = fmap (const (unknown, Nothing))
      decl i t' = AST.AParamDecl (unknown, Just (Type t')) (AST.StellaIdent $ "x" ++ show i) (type' t')
      args = [decl i t' | (i, t') <- zip [0 :: Integer ..] argts]
   in AST.Abstraction (unknown, Just t) args (defaultValue (Type returnt))
defaultValue t@(Type (AST.TypeSum _ inl _)) =
  AST.Inl (unknown, Just t) (defaultValue (Type inl))
defaultValue t@(Type (AST.TypeTuple _ ts)) = do
  let ts' = [defaultValue (Type t') | t' <- ts]
   in AST.Tuple (unknown, Just t) ts'
defaultValue t@(Type (AST.TypeRecord _ fields)) = do
  let bindings = [binding id' t' | (AST.ARecordFieldType _ id' t') <- fields]
      binding id' t' = AST.ABinding (unknown, Just (Type t')) id' (defaultValue (Type t'))
   in AST.Record (unknown, Just t) bindings
defaultValue t@(Type (AST.TypeVariant _ ((AST.AVariantFieldType _ id' (AST.NoTyping _)) : _))) =
  let t' = Type.fromAST' AST.TypeUnit
      data' = AST.SomeExprData (unknown, Just t') (AST.ConstUnit (unknown, Just t'))
   in AST.Variant (unknown, Just t) id' data'
defaultValue t@(Type (AST.TypeVariant _ ((AST.AVariantFieldType _ id' (AST.SomeTyping _ t')) : _))) =
  let t'' = Type.fromAST t'
      data' = AST.SomeExprData (unknown, Just t'') (AST.ConstUnit (unknown, Just t''))
   in AST.Variant (unknown, Just t) id' data'
defaultValue (Type (AST.TypeVariant _ [])) = do
  error "defaultValue from AST.Type': empty variant type"
defaultValue t@(Type (AST.TypeList _ _)) = do
  AST.List (unknown, Just t) []
defaultValue t@(Type (AST.TypeBool _)) =
  AST.ConstFalse (unknown, Just t)
defaultValue t@(Type (AST.TypeNat _)) =
  AST.ConstInt (unknown, Just t) 0
defaultValue t@(Type (AST.TypeUnit _)) =
  AST.ConstUnit (unknown, Just t)
defaultValue _ =
  error "defaultValue from AST.Type': unexpected type"

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
    isSupportedExtension LetPatterns = True
    isSupportedExtension TypeAscriptions = True
    isSupportedExtension NaturalLiterals = True
    isSupportedExtension Predecessor = True
    isSupportedExtension ArithmeticOperators = True
    isSupportedExtension ComparisonOperations = True
    isSupportedExtension LogicalOperators = True
    isSupportedExtension Panic = True
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
