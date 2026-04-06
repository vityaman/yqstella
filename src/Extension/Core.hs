module Extension.Core (Extension (..), Extensions, extensionName, extensionFromName, closure) where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)
import Data.Set (Set)

-- | See [Language Extensions Overview](https://fizruk.github.io/stella/?page=%2Fstella%2Fsite%2Fextensions%2Foverview%2F).
data Extension
  = StructuralPatterns
  | LetBindings
  | LetPatterns
  | LetManyBindings
  | NestedFunctionDeclarations
  | WildcardBinders
  | NaturalLiterals
  | ArithmeticOperators
  | ComparisonOperations
  | NullaryFunctions
  | MultiparameterFunctions
  | CurriedMultiparameterFunctions
  | Sequencing
  | CharType
  | StringType
  | IntegerType
  | FloatingPointTypes
  | DecimalTypes
  | RationalTypes
  | ComplexNumberTypes
  | TypeAscriptions
  | TypeAliases
  | UnitType
  | Pairs
  | Tuples
  | Records
  | SumTypes
  | Variants
  | NullaryVariantLabels
  | Enumerations
  | Lists
  | References
  | ExceptionTypeDeclaration
  | OpenVariantExceptions
  | Exceptions
  | Panic
  | StructuralSubtyping
  | TopType
  | BottomType
  | TypeCast
  | UniversalTypes
  | FixpointCombinator
  | GeneralRecursion
  deriving (Eq, Ord, Show)

type Extensions = Set Extension

extensionNameMap :: Bimap Extension String
extensionNameMap =
  Bimap.fromList
    [ -- Syntactic Sugar and Derived Forms
      (StructuralPatterns, "#structural-patterns"),
      (LetBindings, "#let-bindings"),
      (LetPatterns, "#let-patterns"),
      (LetManyBindings, "#let-many-bindings"),
      (NestedFunctionDeclarations, "#nested-function-declarations"),
      (WildcardBinders, "#wildcard-binders"),
      (NaturalLiterals, "#natural-literals"),
      (ArithmeticOperators, "#arithmetic-operators"),
      (ComparisonOperations, "#comparison-operators"),
      (NullaryFunctions, "#nullary-functions"),
      (MultiparameterFunctions, "#multiparameter-functions"),
      (CurriedMultiparameterFunctions, "#curried-multiparameter-functions"),
      (Sequencing, "#sequencing"),
      -- Base Types
      (CharType, "#char-type"),
      (StringType, "#string-type"),
      (IntegerType, "#integer-type"),
      (FloatingPointTypes, "#floating-point-types"),
      (DecimalTypes, "#decimal-types"),
      (RationalTypes, "#rational-types"),
      (ComplexNumberTypes, "#complex-number-types"),
      -- Simple Types
      (TypeAscriptions, "#type-ascriptions"),
      (TypeAliases, "#type-aliases"),
      (UnitType, "#unit-type"),
      (Pairs, "#pairs"),
      (Tuples, "#tuples"),
      (Records, "#records"),
      (SumTypes, "#sum-types"),
      (Variants, "#variants"),
      (NullaryVariantLabels, "#nullary-variant-labels"),
      (Enumerations, "#enumerations"),
      (Lists, "#lists"),
      (References, "#references"),
      -- Exceptions
      (ExceptionTypeDeclaration, "#exception-type-declaration"),
      (OpenVariantExceptions, "#open-variant-exceptions"),
      (Exceptions, "#exceptions"),
      (Panic, "#panic"),
      -- Subtyping
      (StructuralSubtyping, "#structural-subtyping"),
      (TopType, "#top-type"),
      (BottomType, "#bottom-type"),
      (TypeCast, "#type-cast"),
      -- Universal Types
      (UniversalTypes, "#universal-types"),
      -- Recursion
      (FixpointCombinator, "#fixpoint-combinator"),
      (GeneralRecursion, "#general-recursion")
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

closure :: Extension -> [Extension]
closure Tuples = [Tuples, Pairs]
closure CurriedMultiparameterFunctions = [CurriedMultiparameterFunctions, MultiparameterFunctions]
closure x = [x]
