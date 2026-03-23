module Type.Check (checkTypes) where

import Control.Monad.State (evalState)
import Control.Monad.Writer
import Diagnostic.Core (Diagnostic (severity), Diagnostics, Severity (Error, Fatal))
import Position (Position)
import qualified Syntax.AbsStella as AST
import Type.Annotation (inferType)
import qualified Type.Context as Context

checkTypes :: AST.Program' Position -> Writer Diagnostics (Maybe ())
checkTypes program = do
  let (_, diagnostics) = (evalState . runWriterT . inferType) program Context.empty

  let isFailure Fatal = True
      isFailure Error = True

  tell diagnostics

  return $
    if not (any (isFailure . severity) diagnostics)
      then Just ()
      else Nothing
