module Extension.Activation (activateExtensions, enabledExtensions) where

import Control.Monad (guard)
import Control.Monad.Writer (MonadWriter (tell), Writer)
import Data.Either (lefts, rights)
import Data.Foldable (find, toList)
import Data.List (intercalate)
import qualified Data.Set as Set
import Diagnostic.Code (Code (BAD_EXTENSION, NOT_IMPLEMENTED))
import Diagnostic.Core (Diagnostic, Diagnostics, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange, unknown)
import Extension.Annotation (annotateExtensions)
import Extension.Core (Extension, Extensions, areConflicting, extensionFromName, extensionName)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST

activateExtensions :: Extensions -> AST.Program' Position -> Writer Diagnostics ()
activateExtensions enabled program = do
  tell $ do
    (position, extensions) <- toList $ annotateExtensions program
    let disabled = Set.difference extensions enabled
    guard $ not $ Set.null disabled
    let disabledNames = intercalate ", " (extensionName <$> Set.toList disabled)
        message = "disabled extension usage: " ++ disabledNames
    return (diagnostic Error BAD_EXTENSION (pointRange position) message)

  return ()

enabledExtensions :: AST.Program' Position -> Writer Diagnostics Extensions
enabledExtensions (AST.AProgram _ _ extensions _) = do
  let parse (position, name) =
        either (Left . diagnostic Error BAD_EXTENSION (pointRange position)) Right (extensionFromName name)

      names' = fmap parse $ do
        (AST.AnExtension position names) <- extensions
        (AST.ExtensionName name) <- names
        return (position, name)

      diagnostics' = lefts names'
      extensions' = Set.fromList $ concatMap Extension.closure (rights names')

  tell diagnostics'

  case checkNoConflicting (toList extensions') of
    Left diagnostic' -> tell [diagnostic']
    Right () -> return ()

  return extensions'

checkNoConflicting :: [Extension] -> Either Diagnostic ()
checkNoConflicting es = case find (uncurry areConflicting) [(a, b) | a <- es, b <- es] of
  Just (a, b) ->
    let message = "conflicting extensions " ++ show a ++ " and " ++ show b
     in Left $ diagnostic Error NOT_IMPLEMENTED (pointRange unknown) message
  Nothing ->
    return ()
