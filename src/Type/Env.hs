module Type.Env
  ( TypeAnnotationEnv,
    TypeAnnotator,
    withStateTAE,
    isAvailable,
    positionOf,
    typeOf,
    freshTypeVar,
    validateUniqueBy,
    tellD,
    tellC,
  )
where

import Annotation (Annotated (annotation))
import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Trans.Writer
import Diagnostic.Code (Code (DEBUG))
import Diagnostic.Core (Diagnostic, Diagnostics, Severity (Info), diagnostic)
import Diagnostic.Position (Position, pointRange)
import Extension.Core (Extension (DebugUnification))
import Misc.Duplicate (sepUniqDupBy)
import Type.Constraint (Constraint (Eq), Constraints)
import Type.Context (Context)
import qualified Type.Context as Context
import Type.Core (Type)

type TypeAnnotationEnv a = WriterT (Diagnostics, Constraints) (State Context) a

type TypeAnnotator f = Maybe Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))

withStateTAE :: (Context -> Context) -> TypeAnnotationEnv a -> TypeAnnotationEnv a
withStateTAE f m = do
  old <- get
  put (Context.restoreInferenceState old (f old))
  result <- m
  inferred <- get
  put (Context.restoreInferenceState inferred old)
  return result

isAvailable :: Extension -> TypeAnnotationEnv Bool
isAvailable e = do
  context <- get
  return $ Context.isAvailable context e

positionOf :: (Annotated f) => f (Position, Maybe Type) -> Position
positionOf = fst . annotation

typeOf :: (Annotated f) => f (Position, Maybe Type) -> Maybe Type
typeOf = snd . annotation

freshTypeVar :: TypeAnnotationEnv Type
freshTypeVar = do
  context <- get
  let (t, context') = Context.withFreshTypeVar context
  put context'
  return t

validateUniqueBy :: (Ord k) => Bool -> (a -> k) -> (a -> Diagnostic) -> [a] -> TypeAnnotationEnv [a]
validateUniqueBy reporting key toDiagnostic values = do
  let (unique, duplicates) = sepUniqDupBy key values
  when reporting $ tellD (fmap toDiagnostic duplicates)
  return unique

tellD :: Diagnostics -> TypeAnnotationEnv ()
tellD ds = tell (ds, mempty)

tellC :: Constraints -> TypeAnnotationEnv ()
tellC cs = do
  isDebugUnification <- isAvailable DebugUnification
  when isDebugUnification $ tellD (fmap toDiagnostic cs)
  tell (mempty, cs)
  where
    toDiagnostic constraint@(Eq position _ _) =
      diagnostic Info DEBUG (pointRange position) (show constraint)
