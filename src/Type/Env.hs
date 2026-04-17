module Type.Env
  ( TypeAnnotationEnv,
    TypeAnnotator,
    withStateTAE,
    positionOf,
    typeOf,
  )
where

import Annotation (Annotated (annotation))
import Control.Monad.State
import Control.Monad.Trans.Writer
import Diagnostic.Core (Diagnostics)
import Diagnostic.Position (Position)
import Type.Context (Context)
import Type.Core (Type)

type TypeAnnotationEnv a = WriterT Diagnostics (State Context) a

type TypeAnnotator f = Maybe Type -> f Position -> TypeAnnotationEnv (f (Position, Maybe Type))

withStateTAE :: (Context -> Context) -> TypeAnnotationEnv a -> TypeAnnotationEnv a
withStateTAE f m = do
  old <- get
  modify f
  result <- m
  put old
  return result

positionOf :: (Annotated f) => f (Position, Maybe Type) -> Position
positionOf = fst . annotation

typeOf :: (Annotated f) => f (Position, Maybe Type) -> Maybe Type
typeOf = snd . annotation
