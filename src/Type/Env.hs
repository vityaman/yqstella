module Type.Env
  ( TypeAnnotationEnv,
    TypeAnnotator,
    withStateTAE,
    isAvailable,
    positionOf,
    typeOf,
  )
where

import Annotation (Annotated (annotation))
import Control.Monad.State
import Control.Monad.Trans.Writer
import Diagnostic.Core (Diagnostics)
import Diagnostic.Position (Position)
import Extension.Core (Extension)
import Type.Context (Context)
import qualified Type.Context as Context
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

isAvailable :: Extension -> TypeAnnotationEnv Bool
isAvailable e = do
  context <- get
  return $ Context.isAvailable context e

positionOf :: (Annotated f) => f (Position, Maybe Type) -> Position
positionOf = fst . annotation

typeOf :: (Annotated f) => f (Position, Maybe Type) -> Maybe Type
typeOf = snd . annotation
