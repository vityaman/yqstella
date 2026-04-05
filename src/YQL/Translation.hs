module YQL.Translation (toYQL) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (runWriter)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Diagnostic.Core (Diagnostic, notImplemented)
import Diagnostic.Position (Position, unknown)
import Extension.Activation (enabledExtensions)
import Extension.Core (Extension (..), extensionName)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type (Type))
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

      declare (AST.AParamDecl _ (AST.StellaIdent name') t) = do
        t' <- toYQL t
        return $ Y [A "declare", A $ "__" ++ name' ++ "__", t']

      toMainArg (AST.AParamDecl _ (AST.StellaIdent name') _) = do
        Right $ A $ "__" ++ name' ++ "__"

instance YQLTranslatable AST.Decl' where
  toYQL (AST.DeclFun p _ (AST.StellaIdent name) paramdecls _ (AST.NoThrowType _) [] expr) = do
    lambda' <- toYQL (AST.Abstraction p paramdecls expr)
    return $ Y [A "let", A name, lambda']
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
    return $ Y [A "/MayWarn", lhs', rhs']
  toYQL (AST.If _ condition thenB elseB) = do
    condition' <- toYQL condition
    thenB' <- toYQL thenB
    elseB' <- toYQL elseB
    return $ Y [A "If", condition', thenB', elseB']
  toYQL (AST.Let _ [AST.APatternBinding _ (AST.PatternVar _ (AST.StellaIdent name)) expr] inExpr) = do
    expr' <- toYQL expr
    inExpr' <- toYQL inExpr
    return $ Y [A "block", Q $ Y [Y [A "let", A name, expr'], Y [A "return", inExpr']]]
  toYQL (AST.Abstraction _ paramdecls expr) = do
    paramdecls' <- mapM toYQL paramdecls
    expr' <- toYQL expr
    return $ Y [A "lambda", Q (Y paramdecls'), expr']
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
  toYQL (AST.Var _ (AST.StellaIdent name)) = do
    return $ A name
  toYQL x = Left $ unsupported x "AST.Expr'"

instance YQLTranslatable AST.Type' where
  toYQL (AST.TypeBool _) = Right $ Y [A "DataType", Q $ A "Bool"]
  toYQL (AST.TypeNat _) = Right $ Y [A "DataType", Q $ A "Uint64"]
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
  toYQL x = Left $ unsupported x "AST.Type'"

instance YQLTranslatable AST.MatchCase' where
  toYQL (AST.AMatchCase _ pattern' expr) = do
    recipes' <- recipes pattern'

    t' <- case snd $ annotation expr of
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
recipes (AST.PatternInl _ pattern'') = do
  recipes'' <- recipes pattern''
  let recipes' = fmap (\f x -> f $ Y [A "Guess", x, Q $ A "inl"]) recipes''
  return recipes'
recipes (AST.PatternInr _ pattern'') = do
  recipes'' <- recipes pattern''
  let recipes' = fmap (\f x -> f $ Y [A "Guess", x, Q $ A "inr"]) recipes''
  return recipes'
recipes (AST.PatternVar _ (AST.StellaIdent name)) =
  Right $ Map.singleton name id
recipes x =
  Left $ unsupported x "AST.Pattern'"

checkExtensions :: [Extension] -> Either Diagnostic ()
checkExtensions extensions = case findUnsupported extensions of
  Nothing -> Right ()
  Just e -> Left $ unsupported' unknown ("Extension " ++ extensionName e)
  where
    isSupportedExtension :: Extension -> Bool
    isSupportedExtension UnitType = True
    isSupportedExtension Pairs = True
    isSupportedExtension Tuples = True
    isSupportedExtension Records = True
    isSupportedExtension SumTypes = True
    isSupportedExtension NullaryFunctions = True
    isSupportedExtension MultiparameterFunctions = True
    isSupportedExtension LetBindings = True
    isSupportedExtension TypeAscriptions = True
    isSupportedExtension NaturalLiterals = True
    isSupportedExtension ArithmeticOperators = True
    isSupportedExtension ComparisonOperations = True
    isSupportedExtension _ = False

    findUnsupported :: [Extension] -> Maybe Extension
    findUnsupported = find (not . isSupportedExtension)

unsupported :: (Annotated f) => f (Position, Maybe Type) -> String -> Diagnostic
unsupported f = unsupported' (fst $ annotation f)

unsupported' :: Position -> String -> Diagnostic
unsupported' p reason =
  let message = "YQL translation unsupported: " ++ reason
   in notImplemented p message

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
   in [ print'
      ]
