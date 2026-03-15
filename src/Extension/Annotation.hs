module Extension.Annotation (annotateExtensions) where

import Annotation (annotation)
import qualified Data.Set as Set
import Extension.Core (Extensions)
import qualified Extension.Core as Extension
import qualified Syntax.AbsStella as AST

class ExtensionsAnnotatable f where
  annotateExtensions :: f a -> f (a, Extensions)

instance ExtensionsAnnotatable AST.Program' where
  annotateExtensions (AST.AProgram p languagedecl extensions decls) =
    AST.AProgram (p, p'') languagedecl' extensions' decls'
    where
      p'' = Set.unions [languagedecl'', extensions'', decls'']

      languagedecl'' = (snd . annotation) languagedecl'
      extensions'' = Set.unions $ fmap (snd . annotation) extensions'
      decls'' = Set.unions $ fmap (snd . annotation) decls'

      languagedecl' = annotateExtensions languagedecl
      extensions' = fmap annotateExtensions extensions
      decls' = fmap annotateExtensions decls

instance ExtensionsAnnotatable AST.LanguageDecl' where
  annotateExtensions (AST.LanguageCore p) =
    AST.LanguageCore (p, Set.empty)

instance ExtensionsAnnotatable AST.Extension' where
  annotateExtensions (AST.AnExtension p extensionnames) =
    AST.AnExtension (p, Set.empty) extensionnames

instance ExtensionsAnnotatable AST.Decl' where
  annotateExtensions (AST.DeclFun p annotations stellaident paramdecls returntype throwtype decls expr) =
    AST.DeclFun (p, p'') annotations' stellaident paramdecls' returntype' throwtype' decls' expr'
    where
      p'' = Set.unions [annotations'', paramdecls'', returntype'', throwtype'', decls'', expr'']

      annotations'' = Set.unions $ fmap (snd . annotation) annotations'
      paramdecls'' = Set.unions $ fmap (snd . annotation) paramdecls'
      returntype'' = (snd . annotation) returntype'
      throwtype'' = (snd . annotation) throwtype'
      decls'' = Set.unions $ fmap (snd . annotation) decls'
      expr'' = (snd . annotation) expr'

      annotations' = fmap annotateExtensions annotations
      paramdecls' = fmap annotateExtensions paramdecls
      returntype' = annotateExtensions returntype
      throwtype' = annotateExtensions throwtype
      decls' = fmap annotateExtensions decls
      expr' = annotateExtensions expr
  annotateExtensions (AST.DeclFunGeneric p annotations stellaident stellaidents paramdecls returntype throwtype decls expr) =
    AST.DeclFunGeneric (p, p'') annotations' stellaident stellaidents paramdecls' returntype' throwtype' decls' expr'
    where
      p'' = Set.unions [annotations'', paramdecls'', returntype'', throwtype'', decls'', expr'']

      annotations'' = Set.unions $ fmap (snd . annotation) annotations'
      paramdecls'' = Set.unions $ fmap (snd . annotation) paramdecls'
      returntype'' = (snd . annotation) returntype'
      throwtype'' = (snd . annotation) throwtype'
      decls'' = Set.unions $ fmap (snd . annotation) decls'
      expr'' = (snd . annotation) expr'

      annotations' = fmap annotateExtensions annotations
      paramdecls' = fmap annotateExtensions paramdecls
      returntype' = annotateExtensions returntype
      throwtype' = annotateExtensions throwtype
      decls' = fmap annotateExtensions decls
      expr' = annotateExtensions expr
  annotateExtensions (AST.DeclTypeAlias p stellaident type_) =
    AST.DeclTypeAlias (p, type_'') stellaident type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_
  annotateExtensions (AST.DeclExceptionType p type_) =
    AST.DeclExceptionType (p, type_'') type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_
  annotateExtensions (AST.DeclExceptionVariant p stellaident type_) =
    AST.DeclExceptionVariant (p, type_'') stellaident type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.LocalDecl' where
  annotateExtensions (AST.ALocalDecl p decl) =
    AST.ALocalDecl (p, decl'') decl'
    where
      decl'' = (snd . annotation) decl'
      decl' = annotateExtensions decl

instance ExtensionsAnnotatable AST.Annotation' where
  annotateExtensions (AST.InlineAnnotation p) =
    AST.InlineAnnotation (p, Set.empty)

instance ExtensionsAnnotatable AST.ParamDecl' where
  annotateExtensions (AST.AParamDecl p stellaident type_) =
    AST.AParamDecl (p, type_'') stellaident type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.ReturnType' where
  annotateExtensions (AST.NoReturnType p) =
    AST.NoReturnType (p, Set.empty)
  annotateExtensions (AST.SomeReturnType p type_) =
    AST.SomeReturnType (p, type_'') type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.ThrowType' where
  annotateExtensions (AST.NoThrowType p) =
    AST.NoThrowType (p, Set.empty)
  annotateExtensions (AST.SomeThrowType p types) =
    AST.SomeThrowType (p, types'') types'
    where
      types'' = Set.unions $ fmap (snd . annotation) types'
      types' = fmap annotateExtensions types

instance ExtensionsAnnotatable AST.Type' where
  annotateExtensions (AST.TypeAuto p) =
    AST.TypeAuto (p, Set.empty)
  annotateExtensions (AST.TypeFun p types type_) =
    AST.TypeFun (p, p'') types' type_'
    where
      p'' = Set.unions [types'', type_'']

      types'' = Set.unions $ fmap (snd . annotation) types'
      type_'' = (snd . annotation) type_'

      types' = fmap annotateExtensions types
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeForAll p stellaidents type_) =
    AST.TypeForAll (p, type_'') stellaidents type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeRec p stellaident type_) =
    AST.TypeRec (p, type_'') stellaident type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeSum p type_1 type_2) =
    AST.TypeSum (p, p'') type_1' type_2'
    where
      p'' = Set.insert Extension.SumTypes $ Set.unions [type_1'', type_2'']

      type_1'' = (snd . annotation) type_1'
      type_2'' = (snd . annotation) type_2'

      type_1' = annotateExtensions type_1
      type_2' = annotateExtensions type_2
  annotateExtensions (AST.TypeTuple p types) =
    AST.TypeTuple (p, types'') types'
    where
      types'' = Set.insert extension $ Set.unions $ fmap (snd . annotation) types'
      types' = fmap annotateExtensions types

      extension = if isPair types then Extension.Pairs else Extension.Tuples

      isPair [_, _] = True
      isPair _ = False
  annotateExtensions (AST.TypeRecord p recordfieldtypes) =
    AST.TypeRecord (p, recordfieldtypes'') recordfieldtypes'
    where
      recordfieldtypes'' = Set.insert Extension.Records $ Set.unions $ fmap (snd . annotation) recordfieldtypes'
      recordfieldtypes' = fmap annotateExtensions recordfieldtypes
  annotateExtensions (AST.TypeVariant p variantfieldtypes) =
    AST.TypeVariant (p, variantfieldtypes'') variantfieldtypes'
    where
      variantfieldtypes'' = Set.insert Extension.Variants $ Set.unions $ fmap (snd . annotation) variantfieldtypes'
      variantfieldtypes' = fmap annotateExtensions variantfieldtypes
  annotateExtensions (AST.TypeList p type_) =
    AST.TypeList (p, type_'') type_'
    where
      type_'' = Set.insert Extension.Lists $ (snd . annotation) type_'
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeBool p) =
    AST.TypeBool (p, Set.empty)
  annotateExtensions (AST.TypeNat p) =
    AST.TypeNat (p, Set.empty)
  annotateExtensions (AST.TypeUnit p) =
    AST.TypeUnit (p, Set.singleton Extension.UnitType)
  annotateExtensions (AST.TypeTop p) =
    AST.TypeTop (p, Set.empty)
  annotateExtensions (AST.TypeBottom p) =
    AST.TypeBottom (p, Set.empty)
  annotateExtensions (AST.TypeRef p type_) =
    AST.TypeRef (p, type_'') type_'
    where
      type_'' = (snd . annotation) type_'
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeVar p stellaident) =
    AST.TypeVar (p, Set.empty) stellaident

instance ExtensionsAnnotatable AST.MatchCase' where
  annotateExtensions (AST.AMatchCase p pattern_ expr) =
    AST.AMatchCase (p, p'') pattern_' expr'
    where
      p'' = Set.insert Extension.SumTypes $ Set.unions [pattern_'', expr'']

      pattern_'' = (snd . annotation) pattern_'
      expr'' = (snd . annotation) expr'

      pattern_' = annotateExtensions pattern_
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.OptionalTyping' where
  annotateExtensions (AST.NoTyping p) =
    AST.NoTyping (p, Set.empty)
  annotateExtensions (AST.SomeTyping p type_) =
    AST.SomeTyping (p, type_'') type_'
    where
      type_'' = Set.insert Extension.Variants $ (snd . annotation) type_'
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.PatternData' where
  annotateExtensions (AST.NoPatternData p) =
    AST.NoPatternData (p, Set.empty)
  annotateExtensions (AST.SomePatternData p pattern_) =
    AST.SomePatternData (p, pattern_'') pattern_'
    where
      pattern_'' = Set.insert Extension.Variants $ (snd . annotation) pattern_'
      pattern_' = annotateExtensions pattern_

instance ExtensionsAnnotatable AST.ExprData' where
  annotateExtensions (AST.NoExprData p) =
    AST.NoExprData (p, Set.empty)
  annotateExtensions (AST.SomeExprData p expr) =
    AST.SomeExprData (p, expr'') expr'
    where
      expr'' = Set.insert Extension.Variants $ (snd . annotation) expr'
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.Pattern' where
  annotateExtensions (AST.PatternCastAs p pattern_ type_) =
    AST.PatternCastAs (p, p'') pattern_' type_'
    where
      p'' = Set.unions [pattern_'', type_'']

      pattern_'' = (snd . annotation) pattern_'
      type_'' = (snd . annotation) type_'

      pattern_' = annotateExtensions pattern_
      type_' = annotateExtensions type_
  annotateExtensions (AST.PatternAsc p pattern_ type_) =
    AST.PatternAsc (p, p'') pattern_' type_'
    where
      p'' = Set.unions [pattern_'', type_'']

      pattern_'' = (snd . annotation) pattern_'
      type_'' = (snd . annotation) type_'

      pattern_' = annotateExtensions pattern_
      type_' = annotateExtensions type_
  annotateExtensions (AST.PatternVariant p stellaident patterndata) =
    AST.PatternVariant (p, patterndata'') stellaident patterndata'
    where
      patterndata'' = Set.insert Extension.Variants $ (snd . annotation) patterndata'
      patterndata' = annotateExtensions patterndata
  annotateExtensions (AST.PatternInl p pattern_) =
    AST.PatternInl (p, pattern_'') pattern_'
    where
      pattern_'' = Set.insert Extension.SumTypes $ (snd . annotation) pattern_'
      pattern_' = annotateExtensions pattern_
  annotateExtensions (AST.PatternInr p pattern_) =
    AST.PatternInr (p, pattern_'') pattern_'
    where
      pattern_'' = (snd . annotation) pattern_'
      pattern_' = annotateExtensions pattern_
  annotateExtensions (AST.PatternTuple p patterns) =
    AST.PatternTuple (p, patterns'') patterns'
    where
      patterns'' = Set.unions $ fmap (snd . annotation) patterns'
      patterns' = fmap annotateExtensions patterns
  annotateExtensions (AST.PatternRecord p labelledpatterns) =
    AST.PatternRecord (p, labelledpatterns'') labelledpatterns'
    where
      labelledpatterns'' = Set.unions $ fmap (snd . annotation) labelledpatterns'
      labelledpatterns' = fmap annotateExtensions labelledpatterns
  annotateExtensions (AST.PatternList p patterns) =
    AST.PatternList (p, patterns'') patterns'
    where
      patterns'' = Set.unions $ fmap (snd . annotation) patterns'
      patterns' = fmap annotateExtensions patterns
  annotateExtensions (AST.PatternCons p pattern_1 pattern_2) =
    AST.PatternCons (p, p'') pattern_1' pattern_2'
    where
      p'' = Set.unions [pattern_1'', pattern_2'']

      pattern_1'' = (snd . annotation) pattern_1'
      pattern_2'' = (snd . annotation) pattern_2'

      pattern_1' = annotateExtensions pattern_1
      pattern_2' = annotateExtensions pattern_2
  annotateExtensions (AST.PatternFalse p) =
    AST.PatternFalse (p, Set.empty)
  annotateExtensions (AST.PatternTrue p) =
    AST.PatternTrue (p, Set.empty)
  annotateExtensions (AST.PatternUnit p) =
    AST.PatternUnit (p, Set.empty)
  annotateExtensions (AST.PatternInt p n) =
    AST.PatternInt (p, Set.empty) n
  annotateExtensions (AST.PatternSucc p pattern_) =
    AST.PatternSucc (p, pattern_'') pattern_'
    where
      pattern_'' = (snd . annotation) pattern_'
      pattern_' = annotateExtensions pattern_
  annotateExtensions (AST.PatternVar p stellaident) =
    AST.PatternVar (p, Set.singleton Extension.LetBindings) stellaident

instance ExtensionsAnnotatable AST.LabelledPattern' where
  annotateExtensions (AST.ALabelledPattern p stellaident pattern_) =
    AST.ALabelledPattern (p, pattern_'') stellaident pattern_'
    where
      pattern_'' = (snd . annotation) pattern_'
      pattern_' = annotateExtensions pattern_

instance ExtensionsAnnotatable AST.Binding' where
  annotateExtensions (AST.ABinding p stellaident expr) =
    AST.ABinding (p, expr'') stellaident expr'
    where
      expr'' = Set.insert Extension.Records $ (snd . annotation) expr'
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.Expr' where
  annotateExtensions (AST.Sequence p expr1 expr2) =
    AST.Sequence (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Assign p expr1 expr2) =
    AST.Assign (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.If p expr1 expr2 expr3) =
    AST.If (p, p'') expr1' expr2' expr3'
    where
      p'' = Set.unions [expr1'', expr2'', expr3'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'
      expr3'' = (snd . annotation) expr3'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
      expr3' = annotateExtensions expr3
  annotateExtensions (AST.Let p patternbindings expr) =
    AST.Let (p, p'') patternbindings' expr'
    where
      p'' = Set.insert Extension.LetBindings $ Set.unions [patternbindings'', expr'']

      patternbindings'' = Set.unions $ fmap (snd . annotation) patternbindings'
      expr'' = (snd . annotation) expr'

      patternbindings' = fmap annotateExtensions patternbindings
      expr' = annotateExtensions expr
  annotateExtensions (AST.LetRec p patternbindings expr) =
    AST.LetRec (p, p'') patternbindings' expr'
    where
      p'' = Set.unions [patternbindings'', expr'']

      patternbindings'' = Set.unions $ fmap (snd . annotation) patternbindings'
      expr'' = (snd . annotation) expr'

      patternbindings' = fmap annotateExtensions patternbindings
      expr' = annotateExtensions expr
  annotateExtensions (AST.TypeAbstraction p stellaidents expr) =
    AST.TypeAbstraction (p, expr'') stellaidents expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.LessThan p expr1 expr2) =
    AST.LessThan (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.LessThanOrEqual p expr1 expr2) =
    AST.LessThanOrEqual (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.GreaterThan p expr1 expr2) =
    AST.GreaterThan (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.GreaterThanOrEqual p expr1 expr2) =
    AST.GreaterThanOrEqual (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Equal p expr1 expr2) =
    AST.Equal (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.NotEqual p expr1 expr2) =
    AST.NotEqual (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.TypeAsc p expr type_) =
    AST.TypeAsc (p, p'') expr' type_'
    where
      p'' = Set.insert Extension.TypeAscriptions $ Set.unions [expr'', type_'']

      expr'' = (snd . annotation) expr'
      type_'' = (snd . annotation) type_'

      expr' = annotateExtensions expr
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeCast p expr type_) =
    AST.TypeCast (p, p'') expr' type_'
    where
      p'' = Set.unions [expr'', type_'']

      expr'' = (snd . annotation) expr'
      type_'' = (snd . annotation) type_'

      expr' = annotateExtensions expr
      type_' = annotateExtensions type_
  annotateExtensions (AST.Abstraction p paramdecls expr) =
    AST.Abstraction (p, p'') paramdecls' expr'
    where
      p'' = Set.unions [paramdecls'', expr'']

      paramdecls'' = Set.unions $ fmap (snd . annotation) paramdecls'
      expr'' = (snd . annotation) expr'

      paramdecls' = fmap annotateExtensions paramdecls
      expr' = annotateExtensions expr
  annotateExtensions (AST.Variant p stellaident exprdata) =
    AST.Variant (p, exprdata'') stellaident exprdata'
    where
      exprdata'' = Set.insert Extension.Variants $ (snd . annotation) exprdata'
      exprdata' = annotateExtensions exprdata
  annotateExtensions (AST.Match p expr matchcases) =
    AST.Match (p, p'') expr' matchcases'
    where
      p'' = Set.insert Extension.SumTypes $ Set.unions [expr'', matchcases'']

      expr'' = (snd . annotation) expr'
      matchcases'' = Set.unions $ fmap (snd . annotation) matchcases'

      expr' = annotateExtensions expr
      matchcases' = fmap annotateExtensions matchcases
  annotateExtensions (AST.List p exprs) =
    AST.List (p, exprs'') exprs'
    where
      exprs'' = Set.insert Extension.Lists $ Set.unions $ fmap (snd . annotation) exprs'
      exprs' = fmap annotateExtensions exprs
  annotateExtensions (AST.Add p expr1 expr2) =
    AST.Add (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Subtract p expr1 expr2) =
    AST.Subtract (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.LogicOr p expr1 expr2) =
    AST.LogicOr (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Multiply p expr1 expr2) =
    AST.Multiply (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Divide p expr1 expr2) =
    AST.Divide (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.LogicAnd p expr1 expr2) =
    AST.LogicAnd (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Ref p expr) =
    AST.Ref (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Deref p expr) =
    AST.Deref (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Application p expr exprs) =
    AST.Application (p, p'') expr' exprs'
    where
      p'' = Set.unions [expr'', exprs'']

      expr'' = (snd . annotation) expr'
      exprs'' = Set.unions $ fmap (snd . annotation) exprs'

      expr' = annotateExtensions expr
      exprs' = fmap annotateExtensions exprs
  annotateExtensions (AST.TypeApplication p expr types) =
    AST.TypeApplication (p, p'') expr' types'
    where
      p'' = Set.unions [expr'', types'']

      expr'' = (snd . annotation) expr'
      types'' = Set.unions $ fmap (snd . annotation) types'

      expr' = annotateExtensions expr
      types' = fmap annotateExtensions types
  annotateExtensions (AST.DotRecord p expr stellaident) =
    AST.DotRecord (p, expr'') expr' stellaident
    where
      expr'' = Set.insert Extension.Records $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.DotTuple p expr n) =
    AST.DotTuple (p, expr'') expr' n
    where
      expr'' = Set.insert extension $ (snd . annotation) expr'
      expr' = annotateExtensions expr

      extension = if isPair then Extension.Pairs else Extension.Tuples

      isPair = n == 0 || n == 1
  annotateExtensions (AST.Tuple p exprs) =
    AST.Tuple (p, exprs'') exprs'
    where
      exprs'' = Set.insert extension $ Set.unions $ fmap (snd . annotation) exprs'
      exprs' = fmap annotateExtensions exprs

      extension = if isPair exprs then Extension.Pairs else Extension.Tuples

      isPair [_, _] = True
      isPair _ = False
  annotateExtensions (AST.Record p bindings) =
    AST.Record (p, bindings'') bindings'
    where
      bindings'' = Set.insert Extension.Records $ Set.unions $ fmap (snd . annotation) bindings'
      bindings' = fmap annotateExtensions bindings
  annotateExtensions (AST.ConsList p expr1 expr2) =
    AST.ConsList (p, p'') expr1' expr2'
    where
      p'' = Set.insert Extension.Lists $ Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Head p expr) =
    AST.Head (p, expr'') expr'
    where
      expr'' = Set.insert Extension.Lists $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.IsEmpty p expr) =
    AST.IsEmpty (p, expr'') expr'
    where
      expr'' = Set.insert Extension.Lists $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Tail p expr) =
    AST.Tail (p, expr'') expr'
    where
      expr'' = Set.insert Extension.Lists $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Panic p) =
    AST.Panic (p, Set.empty)
  annotateExtensions (AST.Throw p expr) =
    AST.Throw (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.TryCatch p expr1 pattern_ expr2) =
    AST.TryCatch (p, p'') expr1' pattern_' expr2'
    where
      p'' = Set.unions [expr1'', pattern_'', expr2'']

      expr1'' = (snd . annotation) expr1'
      pattern_'' = (snd . annotation) pattern_'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      pattern_' = annotateExtensions pattern_
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.TryWith p expr1 expr2) =
    AST.TryWith (p, p'') expr1' expr2'
    where
      p'' = Set.unions [expr1'', expr2'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.TryCastAs p expr1 type_ pattern_ expr2 expr3) =
    AST.TryCastAs (p, p'') expr1' type_' pattern_' expr2' expr3'
    where
      p'' = Set.unions [expr1'', type_'', pattern_'', expr2'', expr3'']

      expr1'' = (snd . annotation) expr1'
      type_'' = (snd . annotation) type_'
      pattern_'' = (snd . annotation) pattern_'
      expr2'' = (snd . annotation) expr2'
      expr3'' = (snd . annotation) expr3'

      expr1' = annotateExtensions expr1
      type_' = annotateExtensions type_
      pattern_' = annotateExtensions pattern_
      expr2' = annotateExtensions expr2
      expr3' = annotateExtensions expr3
  annotateExtensions (AST.Inl p expr) =
    AST.Inl (p, expr'') expr'
    where
      expr'' = Set.insert Extension.SumTypes $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Inr p expr) =
    AST.Inr (p, expr'') expr'
    where
      expr'' = Set.insert Extension.SumTypes $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Succ p expr) =
    AST.Succ (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.LogicNot p expr) =
    AST.LogicNot (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Pred p expr) =
    AST.Pred (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.IsZero p expr) =
    AST.IsZero (p, expr'') expr'
    where
      expr'' = (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.Fix p expr) =
    AST.Fix (p, expr'') expr'
    where
      expr'' = Set.insert Extension.FixpointCombinator $ (snd . annotation) expr'
      expr' = annotateExtensions expr
  annotateExtensions (AST.NatRec p expr1 expr2 expr3) =
    AST.NatRec (p, p'') expr1' expr2' expr3'
    where
      p'' = Set.unions [expr1'', expr2'', expr3'']

      expr1'' = (snd . annotation) expr1'
      expr2'' = (snd . annotation) expr2'
      expr3'' = (snd . annotation) expr3'

      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
      expr3' = annotateExtensions expr3
  annotateExtensions (AST.Fold p type_ expr) =
    AST.Fold (p, p'') type_' expr'
    where
      p'' = Set.unions [type_'', expr'']

      type_'' = (snd . annotation) type_'
      expr'' = (snd . annotation) expr'

      type_' = annotateExtensions type_
      expr' = annotateExtensions expr
  annotateExtensions (AST.Unfold p type_ expr) =
    AST.Unfold (p, p'') type_' expr'
    where
      p'' = Set.unions [type_'', expr'']

      type_'' = (snd . annotation) type_'
      expr'' = (snd . annotation) expr'

      type_' = annotateExtensions type_
      expr' = annotateExtensions expr
  annotateExtensions (AST.ConstTrue p) =
    AST.ConstTrue (p, Set.empty)
  annotateExtensions (AST.ConstFalse p) =
    AST.ConstFalse (p, Set.empty)
  annotateExtensions (AST.ConstUnit p) =
    AST.ConstUnit (p, Set.singleton Extension.UnitType)
  annotateExtensions (AST.ConstInt p n) =
    AST.ConstInt (p, Set.empty) n
  annotateExtensions (AST.ConstMemory p memoryaddress) =
    AST.ConstMemory (p, Set.empty) memoryaddress
  annotateExtensions (AST.Var p stellaident) =
    AST.Var (p, Set.empty) stellaident

instance ExtensionsAnnotatable AST.PatternBinding' where
  annotateExtensions (AST.APatternBinding p pattern_ expr) =
    AST.APatternBinding (p, p'') pattern_' expr'
    where
      p'' = Set.insert Extension.LetBindings $ Set.unions [pattern_'', expr'']

      pattern_'' = (snd . annotation) pattern_'
      expr'' = (snd . annotation) expr'

      pattern_' = annotateExtensions pattern_
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.VariantFieldType' where
  annotateExtensions (AST.AVariantFieldType p stellaident optionaltyping) =
    AST.AVariantFieldType (p, optionaltyping'') stellaident optionaltyping'
    where
      optionaltyping'' = Set.insert Extension.Variants $ (snd . annotation) optionaltyping'
      optionaltyping' = annotateExtensions optionaltyping

instance ExtensionsAnnotatable AST.RecordFieldType' where
  annotateExtensions (AST.ARecordFieldType p stellaident type_) =
    AST.ARecordFieldType (p, type_'') stellaident type_'
    where
      type_'' = Set.insert Extension.Records $ (snd . annotation) type_'
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.Typing' where
  annotateExtensions (AST.ATyping p expr type_) =
    AST.ATyping (p, p'') expr' type_'
    where
      p'' = Set.unions [expr'', type_'']

      expr'' = (snd . annotation) expr'
      type_'' = (snd . annotation) type_'

      expr' = annotateExtensions expr
      type_' = annotateExtensions type_
