module Extension.Annotation (annotateExtensions) where

import qualified Data.Set as Set
import Extension.Core (Extensions)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST

class ExtensionsAnnotatable f where
  annotateExtensions :: f a -> f (a, Extensions)

instance ExtensionsAnnotatable AST.Program' where
  annotateExtensions (AST.AProgram p languagedecl extensions decls) =
    AST.AProgram (p, p'') languagedecl' extensions' decls'
    where
      p'' = Set.empty

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
      p'' =
        Set.fromList $
          [Extension.NestedFunctionDeclarations | not (null decls)]
            ++ paramdeclsExtension paramdecls

      annotations' = fmap annotateExtensions annotations
      paramdecls' = fmap annotateExtensions paramdecls
      returntype' = annotateExtensions returntype
      throwtype' = annotateExtensions throwtype
      decls' = fmap annotateExtensions decls
      expr' = annotateExtensions expr
  annotateExtensions (AST.DeclFunGeneric p annotations stellaident stellaidents paramdecls returntype throwtype decls expr) =
    AST.DeclFunGeneric (p, p'') annotations' stellaident stellaidents paramdecls' returntype' throwtype' decls' expr'
    where
      p'' =
        Set.fromList $
          [Extension.NestedFunctionDeclarations | not (null decls)]
            ++ paramdeclsExtension paramdecls
            ++ [Extension.UniversalTypes]

      annotations' = fmap annotateExtensions annotations
      paramdecls' = fmap annotateExtensions paramdecls
      returntype' = annotateExtensions returntype
      throwtype' = annotateExtensions throwtype
      decls' = fmap annotateExtensions decls
      expr' = annotateExtensions expr
  annotateExtensions (AST.DeclTypeAlias p stellaident type_) =
    AST.DeclTypeAlias (p, Set.singleton Extension.TypeAliases) stellaident type_'
    where
      type_' = annotateExtensions type_
  annotateExtensions (AST.DeclExceptionType p type_) =
    AST.DeclExceptionType (p, Set.singleton Extension.ExceptionTypeDeclaration) type_'
    where
      type_' = annotateExtensions type_
  annotateExtensions (AST.DeclExceptionVariant p stellaident type_) =
    AST.DeclExceptionVariant (p, Set.singleton Extension.OpenVariantExceptions) stellaident type_'
    where
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.LocalDecl' where
  annotateExtensions (AST.ALocalDecl p decl) =
    AST.ALocalDecl (p, Set.fromList [Extension.NestedFunctionDeclarations]) decl'
    where
      decl' = annotateExtensions decl

instance ExtensionsAnnotatable AST.Annotation' where
  annotateExtensions (AST.InlineAnnotation p) =
    AST.InlineAnnotation (p, Set.empty)

instance ExtensionsAnnotatable AST.ParamDecl' where
  annotateExtensions (AST.AParamDecl p stellaident type_) =
    AST.AParamDecl (p, Set.empty) stellaident type_'
    where
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.ReturnType' where
  annotateExtensions (AST.NoReturnType p) =
    AST.NoReturnType (p, Set.empty)
  annotateExtensions (AST.SomeReturnType p type_) =
    AST.SomeReturnType (p, Set.empty) type_'
    where
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.ThrowType' where
  annotateExtensions (AST.NoThrowType p) =
    AST.NoThrowType (p, Set.empty)
  annotateExtensions (AST.SomeThrowType p types) =
    AST.SomeThrowType (p, Set.empty) types'
    where
      types' = fmap annotateExtensions types

instance ExtensionsAnnotatable AST.Type' where
  annotateExtensions (AST.TypeAuto p) =
    AST.TypeAuto (p, Set.empty)
  annotateExtensions (AST.TypeFun p types type_) =
    AST.TypeFun (p, Set.empty) types' type_'
    where
      types' = fmap annotateExtensions types
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeForAll p stellaidents type_) =
    AST.TypeForAll (p, Set.empty) stellaidents type_'
    where
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeRec p stellaident type_) =
    AST.TypeRec (p, Set.empty) stellaident type_'
    where
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeSum p type_1 type_2) =
    AST.TypeSum (p, Set.singleton Extension.SumTypes) type_1' type_2'
    where
      type_1' = annotateExtensions type_1
      type_2' = annotateExtensions type_2
  annotateExtensions (AST.TypeTuple p types) =
    AST.TypeTuple (p, Set.singleton extension) types'
    where
      types' = fmap annotateExtensions types

      extension = if isPair types then Extension.Pairs else Extension.Tuples

      isPair [_, _] = True
      isPair _ = False
  annotateExtensions (AST.TypeRecord p recordfieldtypes) =
    AST.TypeRecord (p, Set.singleton Extension.Records) recordfieldtypes'
    where
      recordfieldtypes' = fmap annotateExtensions recordfieldtypes
  annotateExtensions (AST.TypeVariant p variantfieldtypes) =
    AST.TypeVariant (p, Set.singleton Extension.Variants) variantfieldtypes'
    where
      variantfieldtypes' = fmap annotateExtensions variantfieldtypes
  annotateExtensions (AST.TypeList p type_) =
    AST.TypeList (p, Set.singleton Extension.Lists) type_'
    where
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeBool p) =
    AST.TypeBool (p, Set.empty)
  annotateExtensions (AST.TypeNat p) =
    AST.TypeNat (p, Set.empty)
  annotateExtensions (AST.TypeUnit p) =
    AST.TypeUnit (p, Set.singleton Extension.UnitType)
  annotateExtensions (AST.TypeTop p) =
    AST.TypeTop (p, Set.singleton Extension.TopType)
  annotateExtensions (AST.TypeBottom p) =
    AST.TypeBottom (p, Set.singleton Extension.BottomType)
  annotateExtensions (AST.TypeRef p type_) =
    AST.TypeRef (p, Set.singleton Extension.References) type_'
    where
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeVar p stellaident) =
    AST.TypeVar (p, Set.empty) stellaident

instance ExtensionsAnnotatable AST.MatchCase' where
  annotateExtensions (AST.AMatchCase p pattern_ expr) =
    AST.AMatchCase (p, Set.singleton Extension.SumTypes) pattern_' expr'
    where
      pattern_' = annotateExtensions pattern_
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.OptionalTyping' where
  annotateExtensions (AST.NoTyping p) =
    AST.NoTyping (p, Set.empty)
  annotateExtensions (AST.SomeTyping p type_) =
    AST.SomeTyping (p, Set.singleton Extension.Variants) type_'
    where
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.PatternData' where
  annotateExtensions (AST.NoPatternData p) =
    AST.NoPatternData (p, Set.empty)
  annotateExtensions (AST.SomePatternData p pattern_) =
    AST.SomePatternData (p, Set.singleton Extension.Variants) pattern_'
    where
      pattern_' = annotateExtensions pattern_

instance ExtensionsAnnotatable AST.ExprData' where
  annotateExtensions (AST.NoExprData p) =
    AST.NoExprData (p, Set.empty)
  annotateExtensions (AST.SomeExprData p expr) =
    AST.SomeExprData (p, Set.singleton Extension.Variants) expr'
    where
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.Pattern' where
  annotateExtensions (AST.PatternCastAs p pattern_ type_) =
    AST.PatternCastAs (p, Set.singleton Extension.TypeCast) pattern_' type_'
    where
      pattern_' = annotateExtensions pattern_
      type_' = annotateExtensions type_
  annotateExtensions (AST.PatternAsc p pattern_ type_) =
    AST.PatternAsc (p, Set.empty) pattern_' type_'
    where
      pattern_' = annotateExtensions pattern_
      type_' = annotateExtensions type_
  annotateExtensions (AST.PatternVariant p stellaident patterndata) =
    AST.PatternVariant (p, Set.fromList [Extension.Variants, Extension.StructuralPatterns]) stellaident patterndata'
    where
      patterndata' = annotateExtensions patterndata
  annotateExtensions (AST.PatternInl p pattern_) =
    AST.PatternInl (p, Set.fromList [Extension.SumTypes, Extension.StructuralPatterns]) pattern_'
    where
      pattern_' = annotateExtensions pattern_
  annotateExtensions (AST.PatternInr p pattern_) =
    AST.PatternInr (p, Set.fromList [Extension.SumTypes, Extension.StructuralPatterns]) pattern_'
    where
      pattern_' = annotateExtensions pattern_
  annotateExtensions (AST.PatternTuple p patterns) =
    AST.PatternTuple (p, Set.fromList [extension, Extension.LetBindings]) patterns'
    where
      patterns' = fmap annotateExtensions patterns

      extension = if isPair patterns then Extension.Pairs else Extension.Tuples

      isPair [_, _] = True
      isPair _ = False
  annotateExtensions (AST.PatternRecord p labelledpatterns) =
    AST.PatternRecord (p, Set.fromList [Extension.Records, Extension.LetBindings]) labelledpatterns'
    where
      labelledpatterns' = fmap annotateExtensions labelledpatterns
  annotateExtensions (AST.PatternList p patterns) =
    AST.PatternList (p, Set.fromList [Extension.Lists, Extension.StructuralPatterns]) patterns'
    where
      patterns' = fmap annotateExtensions patterns
  annotateExtensions (AST.PatternCons p pattern_1 pattern_2) =
    AST.PatternCons (p, Set.fromList [Extension.Lists, Extension.StructuralPatterns]) pattern_1' pattern_2'
    where
      pattern_1' = annotateExtensions pattern_1
      pattern_2' = annotateExtensions pattern_2
  annotateExtensions (AST.PatternFalse p) =
    AST.PatternFalse (p, Set.fromList [Extension.StructuralPatterns])
  annotateExtensions (AST.PatternTrue p) =
    AST.PatternTrue (p, Set.fromList [Extension.StructuralPatterns])
  annotateExtensions (AST.PatternUnit p) =
    AST.PatternUnit (p, Set.fromList [Extension.LetBindings])
  annotateExtensions (AST.PatternInt p n) =
    AST.PatternInt (p, Set.fromList [Extension.StructuralPatterns]) n
  annotateExtensions (AST.PatternSucc p pattern_) =
    AST.PatternSucc (p, Set.fromList [Extension.NaturalLiterals, Extension.StructuralPatterns]) pattern_'
    where
      pattern_' = annotateExtensions pattern_
  annotateExtensions (AST.PatternVar p stellaident'@(AST.StellaIdent stellaident)) =
    AST.PatternVar (p, Set.fromList $ wildcard ++ [Extension.LetBindings]) stellaident'
    where
      wildcard = [Extension.WildcardBinders | stellaident == "_"]

instance ExtensionsAnnotatable AST.LabelledPattern' where
  annotateExtensions (AST.ALabelledPattern p stellaident pattern_) =
    AST.ALabelledPattern (p, Set.empty) stellaident pattern_'
    where
      pattern_' = annotateExtensions pattern_

instance ExtensionsAnnotatable AST.Binding' where
  annotateExtensions (AST.ABinding p stellaident expr) =
    AST.ABinding (p, Set.singleton Extension.Records) stellaident expr'
    where
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.Expr' where
  annotateExtensions (AST.Sequence p expr1 expr2) =
    AST.Sequence (p, Set.singleton Extension.Sequencing) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Assign p expr1 expr2) =
    AST.Assign (p, Set.empty) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.If p expr1 expr2 expr3) =
    AST.If (p, Set.empty) expr1' expr2' expr3'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
      expr3' = annotateExtensions expr3
  annotateExtensions (AST.Let p patternbindings expr) =
    AST.Let (p, Set.singleton Extension.LetBindings) patternbindings' expr'
    where
      patternbindings' = fmap annotateExtensions patternbindings
      expr' = annotateExtensions expr
  annotateExtensions (AST.LetRec p patternbindings expr) =
    AST.LetRec (p, Set.empty) patternbindings' expr'
    where
      patternbindings' = fmap annotateExtensions patternbindings
      expr' = annotateExtensions expr
  annotateExtensions (AST.TypeAbstraction p stellaidents expr) =
    AST.TypeAbstraction (p, Set.empty) stellaidents expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.LessThan p expr1 expr2) =
    AST.LessThan (p, Set.singleton Extension.ComparisonOperations) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.LessThanOrEqual p expr1 expr2) =
    AST.LessThanOrEqual (p, Set.singleton Extension.ComparisonOperations) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.GreaterThan p expr1 expr2) =
    AST.GreaterThan (p, Set.singleton Extension.ComparisonOperations) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.GreaterThanOrEqual p expr1 expr2) =
    AST.GreaterThanOrEqual (p, Set.singleton Extension.ComparisonOperations) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Equal p expr1 expr2) =
    AST.Equal (p, Set.singleton Extension.ComparisonOperations) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.NotEqual p expr1 expr2) =
    AST.NotEqual (p, Set.singleton Extension.ComparisonOperations) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.TypeAsc p expr type_) =
    AST.TypeAsc (p, Set.singleton Extension.TypeAscriptions) expr' type_'
    where
      expr' = annotateExtensions expr
      type_' = annotateExtensions type_
  annotateExtensions (AST.TypeCast p expr type_) =
    AST.TypeCast (p, Set.empty) expr' type_'
    where
      expr' = annotateExtensions expr
      type_' = annotateExtensions type_
  annotateExtensions (AST.Abstraction p paramdecls expr) =
    AST.Abstraction (p, Set.fromList $ paramdeclsExtension paramdecls) paramdecls' expr'
    where
      paramdecls' = fmap annotateExtensions paramdecls
      expr' = annotateExtensions expr
  annotateExtensions (AST.Variant p stellaident exprdata) =
    AST.Variant (p, Set.singleton Extension.Variants) stellaident exprdata'
    where
      exprdata' = annotateExtensions exprdata
  annotateExtensions (AST.Match p expr matchcases) =
    AST.Match (p, Set.singleton Extension.SumTypes) expr' matchcases'
    where
      expr' = annotateExtensions expr
      matchcases' = fmap annotateExtensions matchcases
  annotateExtensions (AST.List p exprs) =
    AST.List (p, Set.singleton Extension.Lists) exprs'
    where
      exprs' = fmap annotateExtensions exprs
  annotateExtensions (AST.Add p expr1 expr2) =
    AST.Add (p, Set.singleton Extension.ArithmeticOperators) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Subtract p expr1 expr2) =
    AST.Subtract (p, Set.singleton Extension.ArithmeticOperators) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.LogicOr p expr1 expr2) =
    AST.LogicOr (p, Set.empty) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Multiply p expr1 expr2) =
    AST.Multiply (p, Set.singleton Extension.ArithmeticOperators) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Divide p expr1 expr2) =
    AST.Divide (p, Set.singleton Extension.ArithmeticOperators) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.LogicAnd p expr1 expr2) =
    AST.LogicAnd (p, Set.empty) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Ref p expr) =
    AST.Ref (p, Set.singleton Extension.References) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Deref p expr) =
    AST.Deref (p, Set.singleton Extension.References) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Application p expr exprs) =
    AST.Application (p, Set.empty) expr' exprs'
    where
      expr' = annotateExtensions expr
      exprs' = fmap annotateExtensions exprs
  annotateExtensions (AST.TypeApplication p expr types) =
    AST.TypeApplication (p, Set.empty) expr' types'
    where
      expr' = annotateExtensions expr
      types' = fmap annotateExtensions types
  annotateExtensions (AST.DotRecord p expr stellaident) =
    AST.DotRecord (p, Set.singleton Extension.Records) expr' stellaident
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.DotTuple p expr n) =
    AST.DotTuple (p, Set.singleton extension) expr' n
    where
      expr' = annotateExtensions expr

      extension = if isPair then Extension.Pairs else Extension.Tuples
      isPair = n == 0 || n == 1
  annotateExtensions (AST.Tuple p exprs) =
    AST.Tuple (p, Set.singleton extension) exprs'
    where
      exprs' = fmap annotateExtensions exprs

      extension = if isPair exprs then Extension.Pairs else Extension.Tuples

      isPair [_, _] = True
      isPair _ = False
  annotateExtensions (AST.Record p bindings) =
    AST.Record (p, Set.singleton Extension.Records) bindings'
    where
      bindings' = fmap annotateExtensions bindings
  annotateExtensions (AST.ConsList p expr1 expr2) =
    AST.ConsList (p, Set.singleton Extension.Lists) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.Head p expr) =
    AST.Head (p, Set.singleton Extension.Lists) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.IsEmpty p expr) =
    AST.IsEmpty (p, Set.singleton Extension.Lists) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Tail p expr) =
    AST.Tail (p, Set.singleton Extension.Lists) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Panic p) =
    AST.Panic (p, Set.singleton Extension.Panic)
  annotateExtensions (AST.Throw p expr) =
    AST.Throw (p, Set.singleton Extension.Exceptions) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.TryCatch p expr1 pattern_ expr2) =
    AST.TryCatch (p, Set.singleton Extension.Exceptions) expr1' pattern_' expr2'
    where
      expr1' = annotateExtensions expr1
      pattern_' = annotateExtensions pattern_
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.TryWith p expr1 expr2) =
    AST.TryWith (p, Set.singleton Extension.Exceptions) expr1' expr2'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
  annotateExtensions (AST.TryCastAs p expr1 type_ pattern_ expr2 expr3) =
    AST.TryCastAs (p, Set.fromList [Extension.TypeCast, Extension.Exceptions]) expr1' type_' pattern_' expr2' expr3'
    where
      expr1' = annotateExtensions expr1
      type_' = annotateExtensions type_
      pattern_' = annotateExtensions pattern_
      expr2' = annotateExtensions expr2
      expr3' = annotateExtensions expr3
  annotateExtensions (AST.Inl p expr) =
    AST.Inl (p, Set.singleton Extension.SumTypes) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Inr p expr) =
    AST.Inr (p, Set.singleton Extension.SumTypes) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Succ p expr) =
    AST.Succ (p, Set.empty) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.LogicNot p expr) =
    AST.LogicNot (p, Set.empty) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Pred p expr) =
    AST.Pred (p, Set.empty) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.IsZero p expr) =
    AST.IsZero (p, Set.empty) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.Fix p expr) =
    AST.Fix (p, Set.singleton Extension.FixpointCombinator) expr'
    where
      expr' = annotateExtensions expr
  annotateExtensions (AST.NatRec p expr1 expr2 expr3) =
    AST.NatRec (p, Set.empty) expr1' expr2' expr3'
    where
      expr1' = annotateExtensions expr1
      expr2' = annotateExtensions expr2
      expr3' = annotateExtensions expr3
  annotateExtensions (AST.Fold p type_ expr) =
    AST.Fold (p, Set.empty) type_' expr'
    where
      type_' = annotateExtensions type_
      expr' = annotateExtensions expr
  annotateExtensions (AST.Unfold p type_ expr) =
    AST.Unfold (p, Set.empty) type_' expr'
    where
      type_' = annotateExtensions type_
      expr' = annotateExtensions expr
  annotateExtensions (AST.ConstTrue p) =
    AST.ConstTrue (p, Set.empty)
  annotateExtensions (AST.ConstFalse p) =
    AST.ConstFalse (p, Set.empty)
  annotateExtensions (AST.ConstUnit p) =
    AST.ConstUnit (p, Set.singleton Extension.UnitType)
  annotateExtensions (AST.ConstInt p n) =
    AST.ConstInt (p, Set.fromList extensions) n
    where
      extensions = [Extension.NaturalLiterals | n /= 0]
  annotateExtensions (AST.ConstMemory p memoryaddress) =
    AST.ConstMemory (p, Set.empty) memoryaddress
  annotateExtensions (AST.Var p stellaident) =
    AST.Var (p, Set.empty) stellaident

instance ExtensionsAnnotatable AST.PatternBinding' where
  annotateExtensions (AST.APatternBinding p pattern_ expr) =
    AST.APatternBinding (p, Set.singleton Extension.LetBindings) pattern_' expr'
    where
      pattern_' = annotateExtensions pattern_
      expr' = annotateExtensions expr

instance ExtensionsAnnotatable AST.VariantFieldType' where
  annotateExtensions (AST.AVariantFieldType p stellaident optionaltyping) =
    AST.AVariantFieldType (p, Set.singleton Extension.Variants) stellaident optionaltyping'
    where
      optionaltyping' = annotateExtensions optionaltyping

instance ExtensionsAnnotatable AST.RecordFieldType' where
  annotateExtensions (AST.ARecordFieldType p stellaident type_) =
    AST.ARecordFieldType (p, Set.singleton Extension.Records) stellaident type_'
    where
      type_' = annotateExtensions type_

instance ExtensionsAnnotatable AST.Typing' where
  annotateExtensions (AST.ATyping p expr type_) =
    AST.ATyping (p, Set.empty) expr' type_'
    where
      expr' = annotateExtensions expr
      type_' = annotateExtensions type_

paramdeclsExtension :: [AST.ParamDecl' a] -> [Extension.Extension]
paramdeclsExtension [] = [Extension.NullaryFunctions]
paramdeclsExtension [_] = []
paramdeclsExtension _ = [Extension.MultiparameterFunctions]
