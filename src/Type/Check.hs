module Type.Check (checkTypes) where

import Control.Monad.State (evalState)
import Control.Monad.Writer
import Diagnostic.Core (Diagnostic (severity), Diagnostics, isFailure)
import Diagnostic.Position (Position)
import Extension.Core (Extensions)
import qualified SyntaxGen.AbsStella as AST
import Type.Annotation (inferType)
import qualified Type.Context as Context
import Type.Core (Type)
import qualified Type.Substitution as Substitution
import Type.Unification (unify)

checkTypes :: Extensions -> AST.Program' Position -> Writer Diagnostics (Bool, AST.Program' (Position, Maybe Type))
checkTypes extensions program = do
  let (program', (diagnostics, constraints)) = (run . inferType) program (Context.empty extensions)
      run = evalState . runWriterT
      areInferredTypesCorrect = not (any (isFailure . severity) diagnostics)

  tell diagnostics

  if not areInferredTypesCorrect
    then return (False, program')
    else case unify constraints of
      Left diagnostic -> do
        tell [diagnostic]
        return (False, program')
      Right substitution -> do
        let program'' = Substitution.applyProgram substitution program'

        areTypesUnambiguous <- case Substitution.checkAmbiguity substitution of
          Left diagnostic -> do
            tell [diagnostic]
            return False
          Right () ->
            return True

        return (areTypesUnambiguous, program'')
