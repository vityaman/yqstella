module Type.Check (checkTypes) where

import Annotation (Annotated (annotation))
import Control.Monad.State (evalState)
import Control.Monad.Writer
import Data.Functor (void)
import Diagnostic (Diagnostics)
import Position (Position)
import qualified Syntax.AbsStella as AST
import Type.Annotation (annotateType)
import qualified Type.Context as Context

checkTypes :: AST.Program' Position -> Writer Diagnostics (Maybe ())
checkTypes program = do
  let (program', diagnostics) = (evalState . runWriterT . annotateType) program Context.empty
      type' = (snd . annotation) program'
  tell diagnostics
  return $ void type'
