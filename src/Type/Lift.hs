module Type.Lift (liftType, liftType') where

import Diagnostic.Position
import Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST
import Type.Core (Type)
import Type.Env
import Type.Expectation (liftEqType, liftEqType')
import Type.Subtyping (liftSubType, liftSubType')

liftType :: Position -> (() -> AST.Type' ()) -> Maybe Type -> TypeAnnotationEnv Type
liftType p lifting expected = do
  isSubtyping <- isAvailable Extension.StructuralSubtyping
  if isSubtyping
    then liftSubType p lifting expected
    else liftEqType p lifting expected

liftType' :: Position -> Type -> Maybe Type -> TypeAnnotationEnv Type
liftType' p lifting expected = do
  isSubtyping <- isAvailable Extension.StructuralSubtyping
  if isSubtyping
    then liftSubType' p lifting expected
    else liftEqType' p lifting expected
