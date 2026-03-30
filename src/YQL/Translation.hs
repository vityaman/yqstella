module YQL.Translation (toYQL) where

import Annotation (Annotated (annotation))
import Control.Monad.Writer (runWriter)
import Data.Foldable (find)
import qualified Data.Set as Set
import Diagnostic.Core (Diagnostic, notImplemented)
import Diagnostic.Position (Position, unknown)
import Extension.Activation (enabledExtensions)
import Extension.Core (Extension (..), extensionName)
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
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

    let main' =
          prelude "dq"
            ++ topdecls'
            ++ paramdecls'
            ++ [Y [A "let", A "result", Y $ [A "Apply", A "main"] ++ mainargs']]
            ++ [Y [A "let", A "world", Y [A "Apply", A "print", A "world", Y [A "AsList", A "result"]]]]
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
  toYQL (AST.If _ condition thenB elseB) = do
    condition' <- toYQL condition
    thenB' <- toYQL thenB
    elseB' <- toYQL elseB
    return $ Y [A "If", condition', thenB', elseB']
  toYQL (AST.Abstraction _ paramdecls expr) = do
    paramdecls' <- mapM toYQL paramdecls
    expr' <- toYQL expr
    return $ Y [A "lambda", Q (Y paramdecls'), expr']
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
  toYQL (AST.ConstInt _ n) = do
    return $ Y [A "Uint64", Q (A $ show n)]
  toYQL (AST.ConstUnit _) = do
    return $ Y [A "Void"]
  toYQL (AST.Var _ (AST.StellaIdent name)) = do
    return $ A name
  toYQL x = Left $ unsupported x "AST.Expr'"

instance YQLTranslatable AST.Type' where
  toYQL (AST.TypeBool _) = Right $ Y [A "DataType", Q $ A "Bool"]
  toYQL (AST.TypeNat _) = Right $ Y [A "DataType", Q $ A "Uint64"]
  toYQL x = Left $ unsupported x "AST.Type'"

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
    isSupportedExtension NullaryFunctions = True
    isSupportedExtension MultiparameterFunctions = True
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
