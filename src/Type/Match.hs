{-# LANGUAGE TupleSections #-}

module Type.Match (annotateMatchType) where

import Annotation (Annotated (annotation))
import Control.Monad (unless, when, zipWithM)
import Control.Monad.State
import Control.Monad.Writer (tell)
import qualified Data.Bifunctor
import Data.Foldable (find)
import Data.Functor (void)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Diagnostic.Code (Code (..))
import Diagnostic.Core (Diagnostic, Severity (Error), diagnostic, notImplemented)
import Diagnostic.Position (Position, pointRange)
import Misc.Duplicate (sepUniqDupBy)
import Syntax.PrettyPrint (displayAST)
import qualified SyntaxGen.AbsStella as AST
import qualified Type.Context as Context
import Type.Core (Type (Type))
import qualified Type.Core as Type
import Type.Env (TypeAnnotationEnv, TypeAnnotator, typeOf, withStateTAE)
import Type.Expectation (commonType)
import Type.UsefulClause

checkType :: Type -> AST.Pattern' Position -> Either Diagnostic (AST.Pattern' (Position, Type))
checkType _ (AST.PatternCastAs p _ _) =
  Left $ notImplemented p "PatternCastAs Matching"
checkType _ (AST.PatternAsc p _ _) =
  Left $ notImplemented p "PatternAsc Matching"
checkType t@(Type (AST.TypeVariant _ fields)) (AST.PatternVariant p (AST.StellaIdent tag) data') = do
  let tag'' (AST.AVariantFieldType _ (AST.StellaIdent tag') _) = tag'
      entry = find (\x -> tag'' x == tag) fields

  maybeType <- case entry of
    Just (AST.AVariantFieldType _ _ entry') ->
      Right entry'
    Nothing ->
      let message = "tag " ++ tag ++ " not found"
       in Left $ diagnostic Error UNEXPECTED_VARIANT_LABEL (pointRange p) message

  case (data', maybeType) of
    (AST.NoPatternData p', AST.NoTyping _) ->
      Right
        ( AST.PatternVariant
            (p, t)
            (AST.StellaIdent tag)
            (AST.NoPatternData (p', Type.fromAST' AST.TypeUnit))
        )
    (AST.SomePatternData p' pattern', AST.SomeTyping _ t') -> do
      pattern'' <- checkType (Type t') pattern'
      let t'' = snd $ annotation pattern''
      Right
        ( AST.PatternVariant
            (p, t)
            (AST.StellaIdent tag)
            (AST.SomePatternData (p', t'') pattern'')
        )
    (AST.NoPatternData p', AST.SomeTyping _ t') ->
      let message = "unexpected nullary pattern, expected " ++ show (Type t')
       in Left $ diagnostic Error UNEXPECTED_NULLARY_VARIANT_PATTERN (pointRange p') message
    (AST.SomePatternData p' _, AST.NoTyping _) ->
      let message = "unexpected non nullary pattern"
       in Left $ diagnostic Error UNEXPECTED_NON_NULLARY_VARIANT_PATTERN (pointRange p') message
checkType t@(Type (AST.TypeSum _ inl _)) (AST.PatternInl p pattern') = do
  pattern'' <- checkType (Type inl) pattern'
  Right (AST.PatternInl (p, t) pattern'')
checkType t@(Type (AST.TypeSum _ _ inr)) (AST.PatternInr p pattern') = do
  pattern'' <- checkType (Type inr) pattern'
  Right (AST.PatternInr (p, t) pattern'')
checkType t@(Type (AST.TypeTuple _ ts)) (AST.PatternTuple p patterns) = do
  if length ts /= length patterns
    then
      let message = "expected tuple with length " ++ show (length ts) ++ ", got " ++ show (length patterns)
       in Left $ diagnostic Error UNEXPECTED_TUPLE_LENGTH (pointRange p) message
    else do
      patterns' <- zipWithM checkType (fmap Type ts) patterns
      Right (AST.PatternTuple (p, t) patterns')
checkType t@(Type (AST.TypeRecord _ ts)) (AST.PatternRecord p fs) = do
  let expected = Map.fromList [(k, Type v) | AST.ARecordFieldType _ (AST.StellaIdent k) v <- ts]

      name (AST.ALabelledPattern _ (AST.StellaIdent n) _) = n
      (uniq, dups) = sepUniqDupBy name fs
      patternByName = Map.fromList [(name x, x) | x <- uniq]

  () <- case dups of
    (AST.ALabelledPattern p'' (AST.StellaIdent n) _) : _ -> do
      Left $ diagnostic Error DUPLICATE_RECORD_PATTERN_FIELDS (pointRange p'') ("duplicate field: " ++ n)
    [] -> pure ()

  let unexpected = Map.keys $ Map.difference patternByName (void expected)
  unless (null unexpected) $
    let message = "unexpected record fields: " ++ intercalate ", " unexpected
     in Left $ diagnostic Error UNEXPECTED_RECORD_FIELDS (pointRange p) message

  let annotateType (AST.ALabelledPattern p'' n@(AST.StellaIdent fld) pattern') =
        case Map.lookup fld expected of
          Nothing -> do
            let message = "missing record field: " ++ fld
            Left $ diagnostic Error MISSING_RECORD_FIELDS (pointRange p'') message
          Just t'' -> do
            pattern'' <- checkType t'' pattern'
            let t' = snd $ annotation pattern''
            Right (AST.ALabelledPattern (p'', t') n pattern'')

  fs' <- traverse annotateType uniq
  Right (AST.PatternRecord (p, t) fs')
checkType _ (AST.PatternList p _) =
  Left $ notImplemented p "PatternList Matching"
checkType _ (AST.PatternCons p _ _) =
  Left $ notImplemented p "PatternCons Matching"
checkType t@(Type (AST.TypeBool _)) (AST.PatternFalse p) =
  Right (AST.PatternFalse (p, t))
checkType t@(Type (AST.TypeBool _)) (AST.PatternTrue p) =
  Right (AST.PatternTrue (p, t))
checkType t@(Type (AST.TypeUnit _)) (AST.PatternUnit p) =
  Right (AST.PatternUnit (p, t))
checkType t@(Type (AST.TypeNat _)) (AST.PatternInt p n) =
  Right (AST.PatternInt (p, t) n)
checkType t@(Type (AST.TypeNat _)) (AST.PatternSucc p pattern') = do
  pattern'' <- checkType t pattern'
  Right (AST.PatternSucc (p, t) pattern'')
checkType t (AST.PatternVar p (AST.StellaIdent name)) =
  Right (AST.PatternVar (p, t) (AST.StellaIdent name))
checkType t p =
  let position = pointRange $ annotation p
      message = "unexpected pattern for type " ++ show t
   in Left $ diagnostic Error UNEXPECTED_PATTERN_FOR_TYPE position message

bindings :: AST.Pattern' (Position, Type) -> Map String Type
bindings (AST.PatternVariant (_, Type (AST.TypeVariant _ fields)) (AST.StellaIdent tag) data') =
  let tag'' (AST.AVariantFieldType _ (AST.StellaIdent tag') _) = tag'
      entry = fromMaybe (error $ "tag " ++ tag ++ " not found") $ find (\x -> tag'' x == tag) fields
   in case (data', entry) of
        (AST.NoPatternData _, AST.AVariantFieldType _ _ (AST.NoTyping _)) -> Map.empty
        (AST.SomePatternData _ pattern', AST.AVariantFieldType _ _ (AST.SomeTyping _ _)) -> bindings pattern'
        _ -> error "unexpected pattern"
bindings (AST.PatternInl _ pattern') =
  bindings pattern'
bindings (AST.PatternInr _ pattern') =
  bindings pattern'
bindings (AST.PatternTuple _ patterns) =
  Map.unions (map bindings patterns)
bindings (AST.PatternRecord _ fs) =
  Map.unions [bindings p | AST.ALabelledPattern _ _ p <- fs]
bindings (AST.PatternList _ patterns) =
  Map.unions (map bindings patterns)
bindings (AST.PatternCons _ h t) =
  bindings h `Map.union` bindings t
bindings (AST.PatternFalse _) =
  Map.empty
bindings (AST.PatternTrue _) =
  Map.empty
bindings (AST.PatternUnit _) =
  Map.empty
bindings (AST.PatternInt _ _) =
  Map.empty
bindings (AST.PatternSucc _ pattern') =
  bindings pattern'
bindings (AST.PatternVar (_, t) (AST.StellaIdent name)) =
  Map.singleton name t
bindings p =
  error $ "bindings: unexpected pattern " ++ show p

annotateMatchType ::
  Maybe Type ->
  Position ->
  AST.Expr' Position ->
  [AST.MatchCase' Position] ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.Expr' (Position, Maybe Type))
annotateMatchType _ p expr [] annotateType = do
  expr' <- annotateType Nothing expr
  let message = "expected at least one match case"
   in tell [diagnostic Error ILLEGAL_EMPTY_MATCHING (pointRange p) message]
  return (AST.Match (p, Nothing) expr' [])
annotateMatchType t p expr cases annotateType = do
  expr' <- annotateType Nothing expr
  let expr't = typeOf expr'

  cases' <- case expr't of
    Just expr't' -> mapM (\x -> annotateCaseType t x expr't' annotateType) cases
    Nothing -> pure $ fmap (fmap (,Nothing)) cases

  () <- when (all (isJust . typeOf) cases' && isJust expr't) $ do
    let patterns = fmap (fmap snd . (\(AST.AMatchCase _ x _) -> x)) cases'
        patterns' = fmap (fmap (fromMaybe (error "expected type in match case"))) patterns
    case usefulClause patterns' (fromMaybe (error "expected type in match expression") expr't) of
      Just clause ->
        let message = "non-exchaustive pattern-matching, useful clause: " ++ displayAST clause
         in tell [diagnostic Error NONEXHAUSTIVE_MATCH_PATTERNS (pointRange p) message]
      Nothing -> pure ()

  t' <- commonType p (fmap annotation cases')
  return (AST.Match (p, t') expr' cases')

annotateCaseType ::
  Maybe Type ->
  AST.MatchCase' Position ->
  Type ->
  TypeAnnotator AST.Expr' ->
  TypeAnnotationEnv (AST.MatchCase' (Position, Maybe Type))
annotateCaseType t (AST.AMatchCase p pattern' expr) patterntype annotateType = do
  (pattern'', expr') <- case checkType patterntype pattern' of
    Right pattern'' -> do
      context <- get
      let context' = foldr (uncurry Context.withTyped) context (Map.toList $ bindings pattern'')
      expr' <- withStateTAE (const context') (annotateType t expr)
      return (fmap (Data.Bifunctor.second Just) pattern'', expr')
    Left e -> do
      tell [e]
      return (fmap (,Nothing) pattern', fmap (,Nothing) expr)
  let t' = typeOf expr'
  return (AST.AMatchCase (p, t') pattern'' expr')

usefulClause :: [AST.Pattern' Type] -> Type -> Maybe (AST.Pattern' Type)
usefulClause patterns (Type t) =
  let matrix = fmap ((: []) . encode . normalizeP) patterns
      clauses = i ctors [Type (normalizeT t)] matrix
   in case clauses of
        Just [clause] -> Just (decode clause)
        Nothing -> Nothing
        _ -> error $ "Unexpected number of clauses " ++ show (length clauses)

-- TODO: Make ctor tags special `__yqstella`.
-- TODO: Add field names to record patterns.
encode :: AST.Pattern' Type -> PT Type
encode (AST.PatternVariant t (AST.StellaIdent tag) (AST.SomePatternData _ pattern')) =
  C tag [encode pattern'] ::: t
encode (AST.PatternInl t pattern') =
  C "inl" [encode pattern'] ::: t
encode (AST.PatternInr t pattern') =
  C "inr" [encode pattern'] ::: t
encode (AST.PatternTuple t patterns) =
  C "tuple" (map encode patterns) ::: t
encode (AST.PatternRecord t fields) =
  let encode' (AST.ALabelledPattern _ (AST.StellaIdent _) pattern') = encode pattern'
   in C "record" (fmap encode' fields) ::: t
encode (AST.PatternFalse t) =
  C "false" [] ::: t
encode (AST.PatternTrue t) =
  C "true" [] ::: t
encode (AST.PatternUnit t) =
  C "unit" [] ::: t
encode (AST.PatternInt t 0) =
  C "0" [] ::: t
encode (AST.PatternSucc t pattern') =
  C "succ" [encode pattern'] ::: t
encode (AST.PatternVar t _) =
  W ::: t
encode p =
  error $ "encode: unexpected pattern " ++ show p

decode :: PT Type -> AST.Pattern' Type
decode (C "inl" [pattern'] ::: t) =
  AST.PatternInl t (decode pattern')
decode (C "inr" [pattern'] ::: t) =
  AST.PatternInr t (decode pattern')
decode (C "tuple" ps ::: t) =
  AST.PatternTuple t (fmap decode ps)
decode (C "record" fields ::: t) =
  let decode' p@(_ ::: t') =
        AST.ALabelledPattern t' (AST.StellaIdent "?") (decode p)
   in AST.PatternRecord t (fmap decode' fields)
decode (C "false" [] ::: t) =
  AST.PatternFalse t
decode (C "true" [] ::: t) =
  AST.PatternTrue t
decode (C "unit" [] ::: t) =
  AST.PatternUnit t
decode (C "0" [] ::: t) =
  AST.PatternInt t 0
decode (C "succ" [pattern'] ::: t) =
  AST.PatternSucc t (decode pattern')
decode (C tag [pattern'@(_ ::: t')] ::: t) =
  AST.PatternVariant t (AST.StellaIdent tag) (AST.SomePatternData t' (decode pattern'))
decode (W ::: t) =
  AST.PatternVar t (AST.StellaIdent "_")
decode p =
  error $ "decode: unexpected pattern " ++ show p

ctors :: Type -> Map C [Type]
ctors (Type (AST.TypeSum _ inl inr)) =
  Map.fromList [("inl", [Type.fromAST inl]), ("inr", [Type.fromAST inr])]
ctors (Type (AST.TypeTuple _ types)) =
  Map.fromList [("tuple", fmap Type.fromAST types)]
ctors (Type (AST.TypeRecord _ recordfieldtypes)) =
  let ctor' (AST.ARecordFieldType _ (AST.StellaIdent _) type_) = Type.fromAST type_
   in Map.fromList [("record", fmap ctor' recordfieldtypes)]
ctors (Type (AST.TypeVariant _ variantfieldtypes)) =
  Map.fromList
    [ (tag, [Type t])
      | (AST.AVariantFieldType _ (AST.StellaIdent tag) (AST.SomeTyping _ t)) <-
          variantfieldtypes
    ]
ctors (Type (AST.TypeBool _)) =
  Map.fromList [("true", []), ("false", [])]
ctors (Type (AST.TypeNat _)) =
  Map.fromList [("0", []), ("succ", [Type.fromAST' AST.TypeNat])]
ctors (Type (AST.TypeUnit _)) =
  Map.fromList [("unit", [])]
ctors t =
  error $ "ctors: unexpected type " ++ show t

-- TODO: Desugar PatternInt to succ(...0)
-- TODO: Desugar PatternList to cons(..., [])
normalizeP :: AST.Pattern' a -> AST.Pattern' a
normalizeP (AST.PatternVariant p (AST.StellaIdent tag) (AST.NoPatternData p')) =
  AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' (AST.PatternUnit p'))
normalizeP (AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' pattern'')) =
  let pattern' = normalizeP pattern''
   in AST.PatternVariant p (AST.StellaIdent tag) (AST.SomePatternData p' pattern')
normalizeP (AST.PatternInl p pattern'') =
  let pattern' = normalizeP pattern''
   in AST.PatternInl p pattern'
normalizeP (AST.PatternInr p pattern'') =
  let pattern' = normalizeP pattern''
   in AST.PatternInr p pattern'
normalizeP (AST.PatternTuple p patterns) =
  AST.PatternTuple p (fmap normalizeP patterns)
normalizeP (AST.PatternRecord p fields) =
  let withNoTypingAsUnit'' (AST.ALabelledPattern p' n pattern') =
        AST.ALabelledPattern p' n (normalizeP pattern')
   in AST.PatternRecord p (fmap withNoTypingAsUnit'' fields)
normalizeP (AST.PatternList p patterns) =
  AST.PatternList p (fmap normalizeP patterns)
normalizeP (AST.PatternCons p pattern1 pattern2) =
  let pattern1' = normalizeP pattern1
      pattern2' = normalizeP pattern2
   in AST.PatternCons p pattern1' pattern2'
normalizeP (AST.PatternFalse p) =
  AST.PatternFalse p
normalizeP (AST.PatternTrue p) =
  AST.PatternTrue p
normalizeP (AST.PatternUnit p) =
  AST.PatternUnit p
normalizeP (AST.PatternInt p n)
  | 0 <= n =
      iterate (AST.PatternSucc p) (AST.PatternInt p 0) !! fromInteger n
  | otherwise =
      AST.PatternInt p n
normalizeP (AST.PatternSucc p pattern'') =
  let pattern' = normalizeP pattern''
   in AST.PatternSucc p pattern'
normalizeP (AST.PatternVar p stellaident) =
  AST.PatternVar p stellaident
normalizeP (AST.PatternAsc p pattern'' type_) =
  let pattern' = normalizeP pattern''
   in AST.PatternAsc p pattern' type_
normalizeP (AST.PatternCastAs p pattern'' stellaident) =
  let pattern' = normalizeP pattern''
   in AST.PatternCastAs p pattern' stellaident

normalizeT :: AST.Type' a -> AST.Type' a
normalizeT (AST.TypeAuto p) =
  AST.TypeAuto p
normalizeT (AST.TypeFun p types type_) =
  AST.TypeFun p (fmap normalizeT types) (normalizeT type_)
normalizeT (AST.TypeForAll p stellaidents type_) =
  AST.TypeForAll p stellaidents (normalizeT type_)
normalizeT (AST.TypeRec p stellaident type_) =
  AST.TypeRec p stellaident (normalizeT type_)
normalizeT (AST.TypeSum p type_1 type_2) =
  AST.TypeSum p (normalizeT type_1) (normalizeT type_2)
normalizeT (AST.TypeTuple p types) =
  AST.TypeTuple p (fmap normalizeT types)
normalizeT (AST.TypeRecord p recordfieldtypes) =
  let withNoTypingAsUnit'' (AST.ARecordFieldType p' tag t) =
        AST.ARecordFieldType p' tag (normalizeT t)
   in AST.TypeRecord p (fmap withNoTypingAsUnit'' recordfieldtypes)
normalizeT (AST.TypeVariant p variantfieldtypes) =
  let withNoTypingAsUnit'' (AST.AVariantFieldType p' tag (AST.NoTyping p'')) =
        AST.AVariantFieldType p' tag (AST.SomeTyping p'' (AST.TypeUnit p''))
      withNoTypingAsUnit'' (AST.AVariantFieldType p' tag (AST.SomeTyping p'' t'')) =
        let t''' = normalizeT t''
         in AST.AVariantFieldType p' tag (AST.SomeTyping p'' t''')
   in AST.TypeVariant p (fmap withNoTypingAsUnit'' variantfieldtypes)
normalizeT (AST.TypeList p type_) =
  AST.TypeList p (normalizeT type_)
normalizeT (AST.TypeBool p) =
  AST.TypeBool p
normalizeT (AST.TypeNat p) =
  AST.TypeNat p
normalizeT (AST.TypeUnit p) =
  AST.TypeUnit p
normalizeT (AST.TypeTop p) =
  AST.TypeTop p
normalizeT (AST.TypeBottom p) =
  AST.TypeBottom p
normalizeT (AST.TypeRef p type_) =
  AST.TypeRef p (normalizeT type_)
normalizeT (AST.TypeVar p stellaident) =
  AST.TypeVar p stellaident
