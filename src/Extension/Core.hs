module Extension.Core (Extension (..), Extensions, extensionName, extensionFromName) where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)
import Data.Set (Set)

-- | See [Language Extensions Overview](https://fizruk.github.io/stella/?page=%2Fstella%2Fsite%2Fextensions%2Foverview%2F).
data Extension
  = LetBindings
  | NestedFunctionDeclarations
  | WildcardBinders
  | NaturalLiterals
  | ArithmeticOperators
  | ComparisonOperations
  | NullaryFunctions
  | MultiparameterFunctions
  | CurriedMultiparameterFunctions
  | CharType
  | StringType
  | IntegerType
  | FloatingPointTypes
  | DecimalTypes
  | RationalTypes
  | ComplexNumberTypes
  | ModularArithmeticTypes
  | MatrixTypes
  | TypeAscriptions
  | TypeAliases
  | UnitType
  | Pairs
  | Tuples
  | Records
  | SumTypes
  | Variants
  | Enumerations
  | Lists
  | ReferenceTypes
  | MutableArrays
  | Panics
  | BoringExceptions
  | CodeExceptions
  | StringExceptions
  | ClosedVariantExceptions
  | OpenVariantExceptions
  | SubclassExceptions
  | CheckedExceptions
  | Patterns
  | NestedPatterns
  | ExhaustiveChecker
  | PatternSynonyms
  | StructuralSubtyping
  | TopType
  | BottomType
  | Downcasting
  | NumericSubtyping
  | DynamicTypeTests
  | SourceAndSinkReferences
  | IntersectionTypes
  | UnionTypes
  | ImperativeObjects
  | StructuralClasses
  | ClassInstanceVariables
  | SuperclassVariables
  | OpenRecursion
  | NominalClasses
  | EquirecursiveTypes
  | IsorecursiveTypes
  | NominalTypes
  | TypeInference
  | UniversalTypes
  | ImpredicativeTypes
  | LetPolymorphism
  | FixpointCombinator
  deriving (Eq, Ord, Show)

type Extensions = Set Extension

extensionNameMap :: Bimap Extension String
extensionNameMap =
  Bimap.fromList
    [ (LetBindings, "#let-bindings"),
      (NestedFunctionDeclarations, "#nested-function-declarations"),
      (WildcardBinders, "#wildcard-binders"),
      (NaturalLiterals, "#natural-literals"),
      (ArithmeticOperators, "#arithmetic-operators"),
      (ComparisonOperations, "#comparison-operations"),
      (NullaryFunctions, "#nullary-functions"),
      (MultiparameterFunctions, "#multiparameter-functions"),
      (CurriedMultiparameterFunctions, "#curried-multiparameter-functions"),
      (CharType, "#char-type"),
      (StringType, "#string-type"),
      (IntegerType, "#integer-type"),
      (FloatingPointTypes, "#floating-point-types"),
      (DecimalTypes, "#decimal-types"),
      (RationalTypes, "#rational-types"),
      (ComplexNumberTypes, "#complex-number-types"),
      (ModularArithmeticTypes, "#modular-arithmetic-types"),
      (MatrixTypes, "#matrix-types"),
      (TypeAscriptions, "#type-ascriptions"),
      (TypeAliases, "#type-aliases"),
      (UnitType, "#unit-type"),
      (Pairs, "#pairs"),
      (Tuples, "#tuples"),
      (Records, "#records"),
      (SumTypes, "#sum-types"),
      (Variants, "#variants"),
      (Enumerations, "#enumerations"),
      (Lists, "#lists"),
      (ReferenceTypes, "#reference-types"),
      (MutableArrays, "#mutable-arrays"),
      (Panics, "#panics"),
      (BoringExceptions, "#boring-exceptions"),
      (CodeExceptions, "#code-exceptions"),
      (StringExceptions, "#string-exceptions"),
      (ClosedVariantExceptions, "#closed-variant-exceptions"),
      (OpenVariantExceptions, "#open-variant-exceptions"),
      (SubclassExceptions, "#subclass-exceptions"),
      (CheckedExceptions, "#checked-exceptions"),
      (Patterns, "#patterns"),
      (NestedPatterns, "#nested-patterns"),
      (ExhaustiveChecker, "#exhaustive-checker"),
      (PatternSynonyms, "#pattern-synonyms"),
      (StructuralSubtyping, "#structural-subtyping"),
      (TopType, "#top-type"),
      (BottomType, "#bottom-type"),
      (Downcasting, "#downcasting"),
      (NumericSubtyping, "#numeric-subtyping"),
      (DynamicTypeTests, "#dynamic-type-tests"),
      (SourceAndSinkReferences, "#source-and-sink-references"),
      (IntersectionTypes, "#intersection-types"),
      (UnionTypes, "#union-types"),
      (ImperativeObjects, "#imperative-objects"),
      (StructuralClasses, "#structural-classes"),
      (ClassInstanceVariables, "#class-instance-variables"),
      (SuperclassVariables, "#superclass-variables"),
      (OpenRecursion, "#open-recursion"),
      (NominalClasses, "#nominal-classes"),
      (EquirecursiveTypes, "#equirecursive-types"),
      (IsorecursiveTypes, "#isorecursive-types"),
      (NominalTypes, "#nominal-types"),
      (TypeInference, "#type-inference"),
      (UniversalTypes, "#universal-types"),
      (ImpredicativeTypes, "#impredicative-types"),
      (LetPolymorphism, "#let-polymorphism"),
      (FixpointCombinator, "#fixpoint-combinator")
    ]

extensionName :: Extension -> String
extensionName extension =
  fromMaybe
    (error $ "extension " ++ show extension ++ " not found")
    (Bimap.lookup extension extensionNameMap)

extensionFromName :: String -> Either String Extension
extensionFromName name =
  either
    (\_ -> Left $ "unknown extension " ++ name)
    Right
    (Bimap.lookupR name extensionNameMap)
