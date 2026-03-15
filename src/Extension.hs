module Extension (Extension (..)) where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Syntax.AbsStella as AST
import Annotation (annotation)

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
  deriving (Eq, Ord)

class ExtensionsAnnotatable f where
  annotateExtensions :: f a -> f (a, Set Extension)

instance ExtensionsAnnotatable AST.Program' where
  annotateExtensions (AST.AProgram p languagedecl extensions decls) =
    AST.AProgram (p, program'') languagedecl' extensions' decls'
    where
      program'' = Set.unions [languagedecl'', extensions'', decls'']

      languagedecl'' = (snd . annotation) languagedecl'
      extensions'' = Set.unions $ fmap (snd . annotation) extensions'
      decls'' = Set.unions $ fmap (snd . annotation) decls'

      extensions' = fmap annotateExtensions extensions
      languagedecl' = annotateExtensions languagedecl
      decls' = fmap annotateExtensions decls

instance ExtensionsAnnotatable AST.LanguageDecl' where
  annotateExtensions = undefined

instance ExtensionsAnnotatable AST.Extension' where
  annotateExtensions = undefined

instance ExtensionsAnnotatable AST.Decl' where
  annotateExtensions = undefined
