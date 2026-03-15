module Extension.Core (Extension (..), Extensions, extensionName, extensionFromName) where

import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Maybe (fromMaybe)
import Data.Set (Set)

-- | See [Language Extensions Overview](https://fizruk.github.io/stella/?page=%2Fstella%2Fsite%2Fextensions%2Foverview%2F).
data Extension
  = StructuralPatterns
  | LetBindings
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
  | UniversalTypes
  | FixpointCombinator
  deriving (Eq, Ord, Show)

type Extensions = Set Extension

extensionNameMap :: Bimap Extension String
extensionNameMap =
  Bimap.fromList
    [ -- Syntactic Sugar and Derived Forms
      (StructuralPatterns, "#structural-patterns"),
      (LetBindings, "#let-bindings"),
      (NestedFunctionDeclarations, "#nested-function-declarations"),
      (WildcardBinders, "#wildcard-binders"),
      (NaturalLiterals, "#natural-literals"),
      (ArithmeticOperators, "#arithmetic-operators"),
      (ComparisonOperations, "#comparison-operations"),
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
      -- Universal Types
      (UniversalTypes, "#universal-types"),
      -- Other
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

consequences :: Extension -> [Extension]
consequences = undefined
