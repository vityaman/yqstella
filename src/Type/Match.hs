{-# LANGUAGE TupleSections #-}

module Type.Match (expectExhaustive, annotateCaseType) where

import Annotation (Annotated (annotation))
import Control.Monad (unless)
import Control.Monad.State
import Control.Monad.Writer (tell)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Context as Context
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf, withStateTAE)

bindings :: AST.Pattern' Position -> Type -> Either Diagnostic (Map String Type)
bindings (AST.PatternCastAs p _ _) _ =
  Left $ notImplemented p "PatternCastAs Matching"
bindings (AST.PatternAsc p _ _) _ =
  Left $ notImplemented p "PatternAsc Matching"
bindings (AST.PatternVariant p (AST.StellaIdent tag) data') (Type (AST.TypeVariant _ fields)) = do
  let tag'' (AST.AVariantFieldType _ (AST.StellaIdent tag') _) = tag'
      entry = find (\x -> tag'' x == tag) fields

  maybeType <- case entry of
    Just (AST.AVariantFieldType _ _ entry') ->
      Right entry'
    Nothing ->
      let message = ""
       in Left $ diagnostic Error UNEXPECTED_VARIANT_LABEL (pointRange p) message

  case (data', maybeType) of
    (AST.NoPatternData _, AST.NoTyping _) ->
      Right Map.empty
    (AST.NoPatternData p', AST.SomeTyping _ t) ->
      let message = "unexpected nullary pattern, expected " ++ show (Type t)
       in Left $ diagnostic Error UNEXPECTED_NULLARY_VARIANT_PATTERN (pointRange p') message
    (AST.SomePatternData p' _, AST.NoTyping _) ->
      let message = "unexpected non nullary pattern"
       in Left $ diagnostic Error UNEXPECTED_NON_NULLARY_VARIANT_PATTERN (pointRange p') message
    (AST.SomePatternData _ pattern', AST.SomeTyping _ t) ->
      bindings pattern' (Type t)
bindings (AST.PatternInl _ pattern') (Type (AST.TypeSum _ inl _)) =
  bindings pattern' (Type.fromAST inl)
bindings (AST.PatternInr _ pattern') (Type (AST.TypeSum _ _ inr)) =
  bindings pattern' (Type.fromAST inr)
bindings (AST.PatternTuple p _) _ =
  Left $ notImplemented p "PatternTuple Matching"
bindings (AST.PatternRecord p _) _ =
  Left $ notImplemented p "PatternRecord Matching"
bindings (AST.PatternList p _) _ =
  Left $ notImplemented p "PatternList Matching"
bindings (AST.PatternCons p _ _) _ =
  Left $ notImplemented p "PatternCons Matching"
bindings (AST.PatternFalse p) _ =
  Left $ notImplemented p "PatternFalse Matching"
bindings (AST.PatternTrue p) _ =
  Left $ notImplemented p "PatternTrue Matching"
bindings (AST.PatternUnit _) (Type (AST.TypeUnit _)) =
  Right Map.empty
bindings (AST.PatternInt p _) _ =
  Left $ notImplemented p "PatternInt Matching"
bindings (AST.PatternSucc p _) _ =
  Left $ notImplemented p "PatternSucc Matching"
bindings (AST.PatternVar _ (AST.StellaIdent name)) t =
  Right $ Map.singleton name t
bindings p t =
  let position = pointRange $ annotation p
      message = "unexpected pattern for type " ++ show t
   in Left $ diagnostic Error UNEXPECTED_PATTERN_FOR_TYPE position message

annotateCaseType ::
  Maybe Type ->
  AST.MatchCase' Position ->
  Type ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.MatchCase' (Position, Maybe Type))
annotateCaseType t (AST.AMatchCase p pattern' expr) patterntype annotateType = do
  expr' <- case bindings pattern' patterntype of
    Right bindings' -> do
      context <- get
      let context' = foldr (uncurry Context.withTyped) context (Map.toList bindings')
      withStateTAE (const context') (annotateType t expr)
    Left e -> do
      tell [e]
      return $ fmap (,Nothing) expr
  let t' = typeOf expr'
  return (AST.AMatchCase (p, t') (fmap (,Nothing) pattern') expr')

expectExhaustive :: Position -> [AST.MatchCase' (Position, Maybe Type)] -> Maybe Type -> TypeAnnotationEnv ()
expectExhaustive p cases (Just t)
  | all (isJust . typeOf) cases = do
      let patterns = fmap (fmap fst . (\(AST.AMatchCase _ x _) -> x)) cases
          message = "nonexchaustive pattern-matching"
          diagnostic' = diagnostic Error NONEXHAUSTIVE_MATCH_PATTERNS (pointRange p) message
      unless (isExhaustive patterns t) $ tell [diagnostic']
expectExhaustive _ _ _ = pure ()

isExhaustive :: [AST.Pattern' Position] -> Type -> Bool
isExhaustive = undefined

withNoTypingAsUnit' :: AST.Pattern' a -> AST.Pattern' a
withNoTypingAsUnit' (AST.PatternVariant p (AST.StellaIdent tag) (AST.NoPatternData p')) =
  AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' (AST.PatternUnit p'))
withNoTypingAsUnit' (AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' pattern'')) =
  let pattern' = withNoTypingAsUnit' pattern''
   in AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' pattern')
withNoTypingAsUnit' (AST.PatternInl p pattern'') =
  let pattern' = withNoTypingAsUnit' pattern''
   in AST.PatternInl p pattern'
withNoTypingAsUnit' (AST.PatternInr p pattern'') =
  let pattern' = withNoTypingAsUnit' pattern''
   in AST.PatternInr p pattern'
withNoTypingAsUnit' (AST.PatternTuple p patterns) =
  AST.PatternTuple p (fmap withNoTypingAsUnit' patterns)
withNoTypingAsUnit' (AST.PatternRecord p fields) =
  let withNoTypingAsUnit'' (AST.ALabelledPattern p' n pattern') =
        AST.ALabelledPattern p' n (withNoTypingAsUnit' pattern')
   in AST.PatternRecord p (fmap withNoTypingAsUnit'' fields)
withNoTypingAsUnit' (AST.PatternList p patterns) =
  AST.PatternList p (fmap withNoTypingAsUnit' patterns)
withNoTypingAsUnit' (AST.PatternCons p pattern1 pattern2) =
  let pattern1' = withNoTypingAsUnit' pattern1
      pattern2' = withNoTypingAsUnit' pattern2
   in AST.PatternCons p pattern1' pattern2'
withNoTypingAsUnit' (AST.PatternFalse p) =
  AST.PatternFalse p
withNoTypingAsUnit' (AST.PatternTrue p) =
  AST.PatternTrue p
withNoTypingAsUnit' (AST.PatternUnit p) =
  AST.PatternUnit p
withNoTypingAsUnit' (AST.PatternInt p n) =
  AST.PatternInt p n
withNoTypingAsUnit' (AST.PatternSucc p pattern'') =
  let pattern' = withNoTypingAsUnit' pattern''
   in AST.PatternSucc p pattern'
withNoTypingAsUnit' (AST.PatternVar p stellaident) =
  AST.PatternVar p stellaident
withNoTypingAsUnit' (AST.PatternAsc p pattern'' type_) =
  let pattern' = withNoTypingAsUnit' pattern''
   in AST.PatternAsc p pattern' type_
withNoTypingAsUnit' (AST.PatternCastAs p pattern'' stellaident) =
  let pattern' = withNoTypingAsUnit' pattern''
   in AST.PatternCastAs p pattern' stellaident

withNoTypingAsUnit :: AST.Type' a -> AST.Type' a
withNoTypingAsUnit (AST.TypeAuto p) =
  AST.TypeAuto p
withNoTypingAsUnit (AST.TypeFun p types type_) =
  AST.TypeFun p (fmap withNoTypingAsUnit types) (withNoTypingAsUnit type_)
withNoTypingAsUnit (AST.TypeForAll p stellaidents type_) =
  AST.TypeForAll p stellaidents (withNoTypingAsUnit type_)
withNoTypingAsUnit (AST.TypeRec p stellaident type_) =
  AST.TypeRec p stellaident (withNoTypingAsUnit type_)
withNoTypingAsUnit (AST.TypeSum p type_1 type_2) =
  AST.TypeSum p (withNoTypingAsUnit type_1) (withNoTypingAsUnit type_2)
withNoTypingAsUnit (AST.TypeTuple p types) =
  AST.TypeTuple p (fmap withNoTypingAsUnit types)
withNoTypingAsUnit (AST.TypeRecord p recordfieldtypes) =
  let withNoTypingAsUnit'' (AST.ARecordFieldType p' tag t) =
        AST.ARecordFieldType p' tag (withNoTypingAsUnit t)
   in AST.TypeRecord p (fmap withNoTypingAsUnit'' recordfieldtypes)
withNoTypingAsUnit (AST.TypeVariant p variantfieldtypes) =
  let withNoTypingAsUnit'' (AST.AVariantFieldType p' tag (AST.NoTyping p'')) =
        AST.AVariantFieldType p' tag (AST.SomeTyping p'' (AST.TypeUnit p''))
      withNoTypingAsUnit'' (AST.AVariantFieldType p' tag (AST.SomeTyping p'' t'')) =
        let t''' = withNoTypingAsUnit t''
         in AST.AVariantFieldType p' tag (AST.SomeTyping p'' t''')
   in AST.TypeVariant p (fmap withNoTypingAsUnit'' variantfieldtypes)
withNoTypingAsUnit (AST.TypeList p type_) =
  AST.TypeList p (withNoTypingAsUnit type_)
withNoTypingAsUnit (AST.TypeBool p) =
  AST.TypeBool p
withNoTypingAsUnit (AST.TypeNat p) =
  AST.TypeNat p
withNoTypingAsUnit (AST.TypeUnit p) =
  AST.TypeUnit p
withNoTypingAsUnit (AST.TypeTop p) =
  AST.TypeTop p
withNoTypingAsUnit (AST.TypeBottom p) =
  AST.TypeBottom p
withNoTypingAsUnit (AST.TypeRef p type_) =
  AST.TypeRef p (withNoTypingAsUnit type_)
withNoTypingAsUnit (AST.TypeVar p stellaident) =
  AST.TypeVar p stellaident
