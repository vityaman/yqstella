module Type.Check (checkTypes) where

import Control.Monad.State (evalState, runState)
import Control.Monad.Writer
import Diagnostic.Code (Code (OCCURS_CHECK_INFINITE_TYPE))
import Diagnostic.Core (Diagnostic (severity), Diagnostics, isFailure)
import qualified Diagnostic.Core as Diagnostic
import Diagnostic.Position (Position)
import Extension.Core (Extensions)
import qualified SyntaxGen.AbsStella as AST
import Type.Annotation (inferType)
import qualified Type.Context as Context
import Type.Core (Type)
import qualified Type.Substitution as Substitution
import Type.Unification (unify)
import qualified Type.Unification as Unification

checkTypes :: Extensions -> AST.Program' Position -> Writer Diagnostics (Bool, AST.Program' (Position, Maybe Type))
checkTypes extensions program = do
  let initialContext = Context.empty extensions
      ((program', (diagnostics, constraints)), inferredContext) = runState (runWriterT $ inferType program) initialContext
      areInferredTypesCorrect = not (any (isFailure . severity) diagnostics)

  tell diagnostics

  if not areInferredTypesCorrect
    then return (False, program')
    else case unifyPreferOccurs (Context.metaVars inferredContext) constraints of
      Left diagnostic -> do
        tell [diagnostic]
        return (False, program')
      Right substitution -> do
        let (program'', (diagnostic', _)) = evalState (runWriterT $ Substitution.applyProgram substitution program') inferredContext

        tell diagnostic'

        let ambiguityDiagnostics = Unification.checkAmbiguity (Context.metaVars inferredContext) program''
            areTypesUnambiguous = not (any (isFailure . severity) ambiguityDiagnostics)

        tell ambiguityDiagnostics

        return (areTypesUnambiguous, program'')
  where
    unifyPreferOccurs metaVariables constraints =
      case unify metaVariables constraints of
        primary@(Left diagnostic)
          | not (isOccursCheck diagnostic),
            alternative@(Left alternativeDiagnostic) <- unify metaVariables (reverse constraints),
            isOccursCheck alternativeDiagnostic ->
              alternative
          | otherwise -> primary
        result -> result

    isOccursCheck diagnostic = case Diagnostic.code diagnostic of
      OCCURS_CHECK_INFINITE_TYPE -> True
      _ -> False
