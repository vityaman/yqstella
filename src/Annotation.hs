{-# LANGUAGE LambdaCase #-}

module Annotation (Annotated, annotation) where

import SyntaxGen.AbsStella

class Annotated f where
  annotation :: f a -> a

instance Annotated Program' where
  annotation = \case
    AProgram p _ _ _ -> p

instance Annotated LanguageDecl' where
  annotation = \case
    LanguageCore p -> p

instance Annotated Extension' where
  annotation = \case
    AnExtension p _ -> p

instance Annotated Decl' where
  annotation = \case
    DeclFun p _ _ _ _ _ _ _ -> p
    DeclFunGeneric p _ _ _ _ _ _ _ _ -> p
    DeclTypeAlias p _ _ -> p
    DeclExceptionType p _ -> p
    DeclExceptionVariant p _ _ -> p

instance Annotated LocalDecl' where
  annotation = \case
    ALocalDecl p _ -> p

instance Annotated Annotation' where
  annotation = \case
    InlineAnnotation p -> p

instance Annotated ParamDecl' where
  annotation = \case
    AParamDecl p _ _ -> p

instance Annotated ReturnType' where
  annotation = \case
    NoReturnType p -> p
    SomeReturnType p _ -> p

instance Annotated ThrowType' where
  annotation = \case
    NoThrowType p -> p
    SomeThrowType p _ -> p

instance Annotated Type' where
  annotation = \case
    TypeAuto p -> p
    TypeFun p _ _ -> p
    TypeForAll p _ _ -> p
    TypeRec p _ _ -> p
    TypeSum p _ _ -> p
    TypeTuple p _ -> p
    TypeRecord p _ -> p
    TypeVariant p _ -> p
    TypeList p _ -> p
    TypeBool p -> p
    TypeNat p -> p
    TypeUnit p -> p
    TypeTop p -> p
    TypeBottom p -> p
    TypeRef p _ -> p
    TypeVar p _ -> p

instance Annotated MatchCase' where
  annotation = \case
    AMatchCase p _ _ -> p

instance Annotated OptionalTyping' where
  annotation = \case
    NoTyping p -> p
    SomeTyping p _ -> p

instance Annotated PatternData' where
  annotation = \case
    NoPatternData p -> p
    SomePatternData p _ -> p

instance Annotated ExprData' where
  annotation = \case
    NoExprData p -> p
    SomeExprData p _ -> p

instance Annotated Pattern' where
  annotation = \case
    PatternCastAs p _ _ -> p
    PatternAsc p _ _ -> p
    PatternVariant p _ _ -> p
    PatternInl p _ -> p
    PatternInr p _ -> p
    PatternTuple p _ -> p
    PatternRecord p _ -> p
    PatternList p _ -> p
    PatternCons p _ _ -> p
    PatternFalse p -> p
    PatternTrue p -> p
    PatternUnit p -> p
    PatternInt p _ -> p
    PatternSucc p _ -> p
    PatternVar p _ -> p

instance Annotated LabelledPattern' where
  annotation = \case
    ALabelledPattern p _ _ -> p

instance Annotated Binding' where
  annotation = \case
    ABinding p _ _ -> p

instance Annotated Expr' where
  annotation = \case
    Sequence p _ _ -> p
    Assign p _ _ -> p
    If p _ _ _ -> p
    Let p _ _ -> p
    LetRec p _ _ -> p
    TypeAbstraction p _ _ -> p
    LessThan p _ _ -> p
    LessThanOrEqual p _ _ -> p
    GreaterThan p _ _ -> p
    GreaterThanOrEqual p _ _ -> p
    Equal p _ _ -> p
    NotEqual p _ _ -> p
    TypeAsc p _ _ -> p
    TypeCast p _ _ -> p
    Abstraction p _ _ -> p
    Variant p _ _ -> p
    Match p _ _ -> p
    List p _ -> p
    Add p _ _ -> p
    Subtract p _ _ -> p
    LogicOr p _ _ -> p
    Multiply p _ _ -> p
    Divide p _ _ -> p
    LogicAnd p _ _ -> p
    Ref p _ -> p
    Deref p _ -> p
    Application p _ _ -> p
    TypeApplication p _ _ -> p
    DotRecord p _ _ -> p
    DotTuple p _ _ -> p
    Tuple p _ -> p
    Record p _ -> p
    ConsList p _ _ -> p
    Head p _ -> p
    IsEmpty p _ -> p
    Tail p _ -> p
    Panic p -> p
    Throw p _ -> p
    TryCatch p _ _ _ -> p
    TryWith p _ _ -> p
    TryCastAs p _ _ _ _ _ -> p
    Inl p _ -> p
    Inr p _ -> p
    Succ p _ -> p
    LogicNot p _ -> p
    Pred p _ -> p
    IsZero p _ -> p
    Fix p _ -> p
    NatRec p _ _ _ -> p
    Fold p _ _ -> p
    Unfold p _ _ -> p
    ConstTrue p -> p
    ConstFalse p -> p
    ConstUnit p -> p
    ConstInt p _ -> p
    ConstMemory p _ -> p
    Var p _ -> p

instance Annotated PatternBinding' where
  annotation = \case
    APatternBinding p _ _ -> p

instance Annotated VariantFieldType' where
  annotation = \case
    AVariantFieldType p _ _ -> p

instance Annotated RecordFieldType' where
  annotation = \case
    ARecordFieldType p _ _ -> p

instance Annotated Typing' where
  annotation = \case
    ATyping p _ _ -> p
