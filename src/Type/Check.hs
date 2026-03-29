module Type.Check (checkTypes) where

import Control.Monad.State (evalState)
import Control.Monad.Writer
import Diagnostic.Core (Diagnostic (severity), Diagnostics, Severity (Error, Fatal))
import Diagnostic.Position (Position)
import qualified SyntaxGen.AbsStella as AST
import Type.Annotation (inferType)
import qualified Type.Context as Context
import Type.Core (Type)

checkTypes :: AST.Program' Position -> Writer Diagnostics (Bool, AST.Program' (Position, Maybe Type))
checkTypes program = do
  let (program', diagnostics) = (run . inferType) program Context.empty
      run = evalState . runWriterT

      areTypesCorrect = not (any (isFailure . severity) diagnostics)
      isFailure Fatal = True
      isFailure Error = True

  tell diagnostics
  return (areTypesCorrect, program')
