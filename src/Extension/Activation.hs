module Extension.Activation (activateExtensions, enabledExtensions) where

import Control.Monad (guard)
import Control.Monad.Writer (MonadWriter (tell), Writer)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Set as Set
import Diagnostic.Code (Code (BAD_EXTENSION))
import Diagnostic.Core (Diagnostics, Severity (Error), diagnostic)
import Diagnostic.Position (Position, pointRange)
import Extension.Annotation (annotateExtensions)
import Extension.Core (Extensions, extensionFromName, extensionName)
import qualified Extension.Core as Extension
import qualified SyntaxGen.AbsStella as AST

activateExtensions :: AST.Program' Position -> Writer Diagnostics ()
activateExtensions program = do
  enabled <- enabledExtensions program

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
  return extensions'
