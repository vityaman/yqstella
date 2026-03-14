{-
  This file is copy-pasted from the Syntax.PrintStella and
  patched for a better pretty-printing behaviour and annotations
  presentation.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module PrettyPrint (printTree) where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Syntax.AbsStella

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = (++ "\n") . render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

-- Indentation (spaces) for given indentation level.
indent :: Int -> ShowS
indent i = replicateS (2*i) (showChar ' ')

-- Make sure we are on a fresh line.
onNewLine :: Int -> Bool -> ShowS
onNewLine i p = (if p then id else showChar '\n') . indent i

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . rend (i-1) False ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t : ":" : ts -> pending . showString t . showString ": " . rend i False ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, rest) of
      (True , _   , [] ) -> []             -- remove trailing space
      (False, _   , [] ) -> t              -- remove trailing space
      (False, True, _ : _) -> t ++ ' ' : s -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Syntax.AbsStella.StellaIdent where
  prt _ (Syntax.AbsStella.StellaIdent i) = doc $ showString i
instance Print Syntax.AbsStella.ExtensionName where
  prt _ (Syntax.AbsStella.ExtensionName i) = doc $ showString i
instance Print Syntax.AbsStella.MemoryAddress where
  prt _ (Syntax.AbsStella.MemoryAddress i) = doc $ showString i
instance Print (Syntax.AbsStella.Program' a) where
  prt i = \case
    Syntax.AbsStella.AProgram _ languagedecl extensions decls -> prPrec i 0 (concatD [prt 0 languagedecl, prt 0 extensions, prt 0 decls])

instance Print [Syntax.AbsStella.StellaIdent] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.LanguageDecl' a) where
  prt i = \case
    Syntax.AbsStella.LanguageCore _ -> prPrec i 0 (concatD [doc (showString "language"), doc (showString "core"), doc (showString ";"), doc (showChar '\n')])

instance Print (Syntax.AbsStella.Extension' a) where
  prt i = \case
    Syntax.AbsStella.AnExtension _ extensionnames -> prPrec i 0 (concatD [doc (showString "extend"), doc (showString "with"), doc (onNewLine (i + 1) False), prt 0 extensionnames, doc (showChar '\n')])

instance Print [Syntax.AbsStella.ExtensionName] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt i (x:xs) = concatD [prt 0 x, doc (showString ","), doc (onNewLine (i + 1) False), prt 0 xs]

instance Print [Syntax.AbsStella.Extension' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), doc (showChar '\n'), prt 0 xs]

instance Print (Syntax.AbsStella.Decl' a) where
  prt i = \case
    Syntax.AbsStella.DeclFun _ annotations stellaident paramdecls returntype throwtype decls expr -> prPrec i 0 (concatD [prt 0 annotations, doc (showString "fn"), prt 0 stellaident, doc (showString "("), prt 0 paramdecls, doc (showString ")"), prt 0 returntype, prt 0 throwtype, doc (showString "{"), prt 0 decls, doc (showString "return"), prt 0 expr, doc (showString "}"), doc (showChar '\n'), doc (onNewLine (i - 1) False)])
    Syntax.AbsStella.DeclFunGeneric _ annotations stellaident stellaidents paramdecls returntype throwtype decls expr -> prPrec i 0 (concatD [prt 0 annotations, doc (showString "generic"), doc (showString "fn"), prt 0 stellaident, doc (showString "["), prt 0 stellaidents, doc (showString "]"), doc (showString "("), prt 0 paramdecls, doc (showString ")"), prt 0 returntype, prt 0 throwtype, doc (showString "{"), prt 0 decls, doc (showString "return"), prt 0 expr, doc (showString "}")])
    Syntax.AbsStella.DeclTypeAlias _ stellaident type_ -> prPrec i 0 (concatD [doc (showString "type"), prt 0 stellaident, doc (showString "="), prt 0 type_])
    Syntax.AbsStella.DeclExceptionType _ type_ -> prPrec i 0 (concatD [doc (showString "exception"), doc (showString "type"), doc (showString "="), prt 0 type_])
    Syntax.AbsStella.DeclExceptionVariant _ stellaident type_ -> prPrec i 0 (concatD [doc (showString "exception"), doc (showString "variant"), prt 0 stellaident, doc (showString ":"), prt 0 type_])

instance Print [Syntax.AbsStella.Decl' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Syntax.AbsStella.LocalDecl' a) where
  prt i = \case
    Syntax.AbsStella.ALocalDecl _ decl -> prPrec i 0 (concatD [prt 0 decl])

instance Print [Syntax.AbsStella.LocalDecl' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ";"), prt 0 xs]

instance Print (Syntax.AbsStella.Annotation' a) where
  prt i = \case
    Syntax.AbsStella.InlineAnnotation _ -> prPrec i 0 (concatD [doc (showString "inline")])

instance Print [Syntax.AbsStella.Annotation' a] where
  prt _ [] = concatD []
  prt _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print (Syntax.AbsStella.ParamDecl' a) where
  prt i = \case
    Syntax.AbsStella.AParamDecl _ stellaident type_ -> prPrec i 0 (concatD [prt 0 stellaident, doc (showString ":"), prt 0 type_])

instance Print [Syntax.AbsStella.ParamDecl' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.ReturnType' a) where
  prt i = \case
    Syntax.AbsStella.NoReturnType _ -> prPrec i 0 (concatD [])
    Syntax.AbsStella.SomeReturnType _ type_ -> prPrec i 0 (concatD [doc (showString "->"), prt 0 type_])

instance Print (Syntax.AbsStella.ThrowType' a) where
  prt i = \case
    Syntax.AbsStella.NoThrowType _ -> prPrec i 0 (concatD [])
    Syntax.AbsStella.SomeThrowType _ types -> prPrec i 0 (concatD [doc (showString "throws"), prt 9 types])

instance Print (Syntax.AbsStella.Type' a) where
  prt i = \case
    Syntax.AbsStella.TypeAuto _ -> prPrec i 0 (concatD [doc (showString "auto")])
    Syntax.AbsStella.TypeFun _ types type_ -> prPrec i 0 (concatD [doc (showString "fn"), doc (showString "("), prt 0 types, doc (showString ")"), doc (showString "->"), prt 0 type_])
    Syntax.AbsStella.TypeForAll _ stellaidents type_ -> prPrec i 0 (concatD [doc (showString "forall"), prt 0 stellaidents, doc (showString "."), prt 0 type_])
    Syntax.AbsStella.TypeRec _ stellaident type_ -> prPrec i 0 (concatD [doc (showString "\181"), prt 0 stellaident, doc (showString "."), prt 0 type_])
    Syntax.AbsStella.TypeSum _ type_1 type_2 -> prPrec i 1 (concatD [prt 2 type_1, doc (showString "+"), prt 2 type_2])
    Syntax.AbsStella.TypeTuple _ types -> prPrec i 2 (concatD [doc (showString "{"), prt 0 types, doc (showString "}")])
    Syntax.AbsStella.TypeRecord _ recordfieldtypes -> prPrec i 2 (concatD [doc (showString "{"), prt 0 recordfieldtypes, doc (showString "}")])
    Syntax.AbsStella.TypeVariant _ variantfieldtypes -> prPrec i 2 (concatD [doc (showString "<|"), prt 0 variantfieldtypes, doc (showString "|>")])
    Syntax.AbsStella.TypeList _ type_ -> prPrec i 2 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])
    Syntax.AbsStella.TypeBool _ -> prPrec i 3 (concatD [doc (showString "Bool")])
    Syntax.AbsStella.TypeNat _ -> prPrec i 3 (concatD [doc (showString "Nat")])
    Syntax.AbsStella.TypeUnit _ -> prPrec i 3 (concatD [doc (showString "Unit")])
    Syntax.AbsStella.TypeTop _ -> prPrec i 3 (concatD [doc (showString "Top")])
    Syntax.AbsStella.TypeBottom _ -> prPrec i 3 (concatD [doc (showString "Bot")])
    Syntax.AbsStella.TypeRef _ type_ -> prPrec i 3 (concatD [doc (showString "&"), prt 2 type_])
    Syntax.AbsStella.TypeVar _ stellaident -> prPrec i 3 (concatD [prt 0 stellaident])

instance Print [Syntax.AbsStella.Type' a] where
  prt 9 [x] = concatD [prt 9 x]
  prt 9 (x:xs) = concatD [prt 9 x, doc (showString ","), prt 9 xs]
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.MatchCase' a) where
  prt i = \case
    Syntax.AbsStella.AMatchCase _ pattern_ expr -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "=>"), prt 0 expr])

instance Print [Syntax.AbsStella.MatchCase' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString "|"), prt 0 xs]

instance Print (Syntax.AbsStella.OptionalTyping' a) where
  prt i = \case
    Syntax.AbsStella.NoTyping _ -> prPrec i 0 (concatD [])
    Syntax.AbsStella.SomeTyping _ type_ -> prPrec i 0 (concatD [doc (showString ":"), prt 0 type_])

instance Print (Syntax.AbsStella.PatternData' a) where
  prt i = \case
    Syntax.AbsStella.NoPatternData _ -> prPrec i 0 (concatD [])
    Syntax.AbsStella.SomePatternData _ pattern_ -> prPrec i 0 (concatD [doc (showString "="), prt 0 pattern_])

instance Print (Syntax.AbsStella.ExprData' a) where
  prt i = \case
    Syntax.AbsStella.NoExprData _ -> prPrec i 0 (concatD [])
    Syntax.AbsStella.SomeExprData _ expr -> prPrec i 0 (concatD [doc (showString "="), prt 0 expr])

instance Print (Syntax.AbsStella.Pattern' a) where
  prt i = \case
    Syntax.AbsStella.PatternCastAs _ pattern_ type_ -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "cast"), doc (showString "as"), prt 0 type_])
    Syntax.AbsStella.PatternAsc _ pattern_ type_ -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "as"), prt 0 type_])
    Syntax.AbsStella.PatternVariant _ stellaident patterndata -> prPrec i 0 (concatD [doc (showString "<|"), prt 0 stellaident, prt 0 patterndata, doc (showString "|>")])
    Syntax.AbsStella.PatternInl _ pattern_ -> prPrec i 0 (concatD [doc (showString "inl"), doc (showString "("), prt 0 pattern_, doc (showString ")")])
    Syntax.AbsStella.PatternInr _ pattern_ -> prPrec i 0 (concatD [doc (showString "inr"), doc (showString "("), prt 0 pattern_, doc (showString ")")])
    Syntax.AbsStella.PatternTuple _ patterns -> prPrec i 0 (concatD [doc (showString "{"), prt 0 patterns, doc (showString "}")])
    Syntax.AbsStella.PatternRecord _ labelledpatterns -> prPrec i 0 (concatD [doc (showString "{"), prt 0 labelledpatterns, doc (showString "}")])
    Syntax.AbsStella.PatternList _ patterns -> prPrec i 0 (concatD [doc (showString "["), prt 0 patterns, doc (showString "]")])
    Syntax.AbsStella.PatternCons _ pattern_1 pattern_2 -> prPrec i 0 (concatD [doc (showString "cons"), doc (showString "("), prt 0 pattern_1, doc (showString ","), prt 0 pattern_2, doc (showString ")")])
    Syntax.AbsStella.PatternFalse _ -> prPrec i 0 (concatD [doc (showString "false")])
    Syntax.AbsStella.PatternTrue _ -> prPrec i 0 (concatD [doc (showString "true")])
    Syntax.AbsStella.PatternUnit _ -> prPrec i 0 (concatD [doc (showString "unit")])
    Syntax.AbsStella.PatternInt _ n -> prPrec i 0 (concatD [prt 0 n])
    Syntax.AbsStella.PatternSucc _ pattern_ -> prPrec i 0 (concatD [doc (showString "succ"), doc (showString "("), prt 0 pattern_, doc (showString ")")])
    Syntax.AbsStella.PatternVar _ stellaident -> prPrec i 0 (concatD [prt 0 stellaident])

instance Print [Syntax.AbsStella.Pattern' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.LabelledPattern' a) where
  prt i = \case
    Syntax.AbsStella.ALabelledPattern _ stellaident pattern_ -> prPrec i 0 (concatD [prt 0 stellaident, doc (showString "="), prt 0 pattern_])

instance Print [Syntax.AbsStella.LabelledPattern' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.Binding' a) where
  prt i = \case
    Syntax.AbsStella.ABinding _ stellaident expr -> prPrec i 0 (concatD [prt 0 stellaident, doc (showString "="), prt 0 expr])

instance Print [Syntax.AbsStella.Binding' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.Expr' a) where
  prt i = \case
    Syntax.AbsStella.Sequence _ expr1 expr2 -> prPrec i 0 (concatD [prt 1 expr1, doc (showString ";"), prt 0 expr2])
    Syntax.AbsStella.Assign _ expr1 expr2 -> prPrec i 1 (concatD [prt 2 expr1, doc (showString ":="), prt 1 expr2])
    Syntax.AbsStella.If _ expr1 expr2 expr3 -> prPrec i 1 (concatD [doc (showString "if"), prt 1 expr1, doc (showString "then"), prt 1 expr2, doc (showString "else"), prt 1 expr3])
    Syntax.AbsStella.Let _ patternbindings expr -> prPrec i 0 (concatD [doc (showString "let"), prt 0 patternbindings, doc (showString "in"), prt 0 expr])
    Syntax.AbsStella.LetRec _ patternbindings expr -> prPrec i 0 (concatD [doc (showString "letrec"), prt 0 patternbindings, doc (showString "in"), prt 0 expr])
    Syntax.AbsStella.TypeAbstraction _ stellaidents expr -> prPrec i 0 (concatD [doc (showString "generic"), doc (showString "["), prt 0 stellaidents, doc (showString "]"), prt 0 expr])
    Syntax.AbsStella.LessThan _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString "<"), prt 3 expr2])
    Syntax.AbsStella.LessThanOrEqual _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString "<="), prt 3 expr2])
    Syntax.AbsStella.GreaterThan _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString ">"), prt 3 expr2])
    Syntax.AbsStella.GreaterThanOrEqual _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString ">="), prt 3 expr2])
    Syntax.AbsStella.Equal _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString "=="), prt 3 expr2])
    Syntax.AbsStella.NotEqual _ expr1 expr2 -> prPrec i 2 (concatD [prt 3 expr1, doc (showString "!="), prt 3 expr2])
    Syntax.AbsStella.TypeAsc _ expr type_ -> prPrec i 3 (concatD [prt 3 expr, doc (showString "as"), prt 2 type_])
    Syntax.AbsStella.TypeCast _ expr type_ -> prPrec i 3 (concatD [prt 3 expr, doc (showString "cast"), doc (showString "as"), prt 2 type_])
    Syntax.AbsStella.Abstraction _ paramdecls expr -> prPrec i 3 (concatD [doc (showString "fn"), doc (showString "("), prt 0 paramdecls, doc (showString ")"), doc (showString "{"), doc (showString "return"), prt 0 expr, doc (showString "}")])
    Syntax.AbsStella.Variant _ stellaident exprdata -> prPrec i 3 (concatD [doc (showString "<|"), prt 0 stellaident, prt 0 exprdata, doc (showString "|>")])
    Syntax.AbsStella.Match _ expr matchcases -> prPrec i 3 (concatD [doc (showString "match"), prt 2 expr, doc (showString "{"), prt 0 matchcases, doc (showString "}")])
    Syntax.AbsStella.List _ exprs -> prPrec i 3 (concatD [doc (showString "["), prt 0 exprs, doc (showString "]")])
    Syntax.AbsStella.Add _ expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "+"), prt 4 expr2])
    Syntax.AbsStella.Subtract _ expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "-"), prt 4 expr2])
    Syntax.AbsStella.LogicOr _ expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "or"), prt 4 expr2])
    Syntax.AbsStella.Multiply _ expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "*"), prt 5 expr2])
    Syntax.AbsStella.Divide _ expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "/"), prt 5 expr2])
    Syntax.AbsStella.LogicAnd _ expr1 expr2 -> prPrec i 4 (concatD [prt 4 expr1, doc (showString "and"), prt 5 expr2])
    Syntax.AbsStella.Ref _ expr -> prPrec i 5 (concatD [doc (showString "new"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Deref _ expr -> prPrec i 5 (concatD [doc (showString "*"), prt 5 expr])
    Syntax.AbsStella.Application _ expr exprs -> prPrec i 6 (concatD [prt 6 expr, doc (showString "("), prt 0 exprs, doc (showString ")")])
    Syntax.AbsStella.TypeApplication _ expr types -> prPrec i 6 (concatD [prt 6 expr, doc (showString "["), prt 0 types, doc (showString "]")])
    Syntax.AbsStella.DotRecord _ expr stellaident -> prPrec i 6 (concatD [prt 6 expr, doc (showString "."), prt 0 stellaident])
    Syntax.AbsStella.DotTuple _ expr n -> prPrec i 6 (concatD [prt 6 expr, doc (showString "."), prt 0 n])
    Syntax.AbsStella.Tuple _ exprs -> prPrec i 6 (concatD [doc (showString "{"), prt 0 exprs, doc (showString "}")])
    Syntax.AbsStella.Record _ bindings -> prPrec i 6 (concatD [doc (showString "{"), prt 0 bindings, doc (showString "}")])
    Syntax.AbsStella.ConsList _ expr1 expr2 -> prPrec i 6 (concatD [doc (showString "cons"), doc (showString "("), prt 0 expr1, doc (showString ","), prt 0 expr2, doc (showString ")")])
    Syntax.AbsStella.Head _ expr -> prPrec i 6 (concatD [doc (showString "List::head"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.IsEmpty _ expr -> prPrec i 6 (concatD [doc (showString "List::isempty"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Tail _ expr -> prPrec i 6 (concatD [doc (showString "List::tail"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Panic _ -> prPrec i 6 (concatD [doc (showString "panic!")])
    Syntax.AbsStella.Throw _ expr -> prPrec i 6 (concatD [doc (showString "throw"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.TryCatch _ expr1 pattern_ expr2 -> prPrec i 6 (concatD [doc (showString "try"), doc (showString "{"), prt 0 expr1, doc (showString "}"), doc (showString "catch"), doc (showString "{"), prt 0 pattern_, doc (showString "=>"), prt 0 expr2, doc (showString "}")])
    Syntax.AbsStella.TryWith _ expr1 expr2 -> prPrec i 6 (concatD [doc (showString "try"), doc (showString "{"), prt 0 expr1, doc (showString "}"), doc (showString "with"), doc (showString "{"), prt 0 expr2, doc (showString "}")])
    Syntax.AbsStella.TryCastAs _ expr1 type_ pattern_ expr2 expr3 -> prPrec i 6 (concatD [doc (showString "try"), doc (showString "{"), prt 0 expr1, doc (showString "}"), doc (showString "cast"), doc (showString "as"), prt 0 type_, doc (showString "{"), prt 0 pattern_, doc (showString "=>"), prt 0 expr2, doc (showString "}"), doc (showString "with"), doc (showString "{"), prt 0 expr3, doc (showString "}")])
    Syntax.AbsStella.Inl _ expr -> prPrec i 6 (concatD [doc (showString "inl"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Inr _ expr -> prPrec i 6 (concatD [doc (showString "inr"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Succ _ expr -> prPrec i 6 (concatD [doc (showString "succ"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.LogicNot _ expr -> prPrec i 6 (concatD [doc (showString "not"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Pred _ expr -> prPrec i 6 (concatD [doc (showString "Nat::pred"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.IsZero _ expr -> prPrec i 6 (concatD [doc (showString "Nat::iszero"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.Fix _ expr -> prPrec i 6 (concatD [doc (showString "fix"), doc (showString "("), prt 0 expr, doc (showString ")")])
    Syntax.AbsStella.NatRec _ expr1 expr2 expr3 -> prPrec i 6 (concatD [doc (showString "Nat::rec"), doc (showString "("), prt 0 expr1, doc (showString ","), prt 0 expr2, doc (showString ","), prt 0 expr3, doc (showString ")")])
    Syntax.AbsStella.Fold _ type_ expr -> prPrec i 6 (concatD [doc (showString "fold"), doc (showString "["), prt 0 type_, doc (showString "]"), prt 7 expr])
    Syntax.AbsStella.Unfold _ type_ expr -> prPrec i 6 (concatD [doc (showString "unfold"), doc (showString "["), prt 0 type_, doc (showString "]"), prt 7 expr])
    Syntax.AbsStella.ConstTrue _ -> prPrec i 7 (concatD [doc (showString "true")])
    Syntax.AbsStella.ConstFalse _ -> prPrec i 7 (concatD [doc (showString "false")])
    Syntax.AbsStella.ConstUnit _ -> prPrec i 7 (concatD [doc (showString "unit")])
    Syntax.AbsStella.ConstInt _ n -> prPrec i 7 (concatD [prt 0 n])
    Syntax.AbsStella.ConstMemory _ memoryaddress -> prPrec i 7 (concatD [prt 0 memoryaddress])
    Syntax.AbsStella.Var _ stellaident -> prPrec i 7 (concatD [prt 0 stellaident])

instance Print [Syntax.AbsStella.Expr' a] where
  prt 2 [x] = concatD [prt 2 x, doc (showString ";")]
  prt 2 (x:xs) = concatD [prt 2 x, doc (showString ";"), prt 2 xs]
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.PatternBinding' a) where
  prt i = \case
    Syntax.AbsStella.APatternBinding _ pattern_ expr -> prPrec i 0 (concatD [prt 0 pattern_, doc (showString "="), prt 0 expr])

instance Print [Syntax.AbsStella.PatternBinding' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.VariantFieldType' a) where
  prt i = \case
    Syntax.AbsStella.AVariantFieldType _ stellaident optionaltyping -> prPrec i 0 (concatD [prt 0 stellaident, prt 0 optionaltyping])

instance Print [Syntax.AbsStella.VariantFieldType' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.RecordFieldType' a) where
  prt i = \case
    Syntax.AbsStella.ARecordFieldType _ stellaident type_ -> prPrec i 0 (concatD [prt 0 stellaident, doc (showString ":"), prt 0 type_])

instance Print [Syntax.AbsStella.RecordFieldType' a] where
  prt _ [] = concatD []
  prt _ [x] = concatD [prt 0 x]
  prt _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print (Syntax.AbsStella.Typing' a) where
  prt i = \case
    Syntax.AbsStella.ATyping _ expr type_ -> prPrec i 0 (concatD [prt 0 expr, doc (showString ":"), prt 0 type_])
