module Type.UsefulClause (C, P (W, C), PT ((:::)), i) where

import Control.Applicative (asum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

type C = String

data P t
  = W
  | C C [PT t]
  deriving (Show, Eq, Ord)

data PT t = P t ::: t
  deriving (Show, Eq, Ord)

type M t = [[PT t]]

typeOf :: PT t -> t
typeOf (_ ::: t') = t'

s :: C -> [t] -> M t -> M t
s c ts = mapMaybe s'
  where
    s' ((C c' args ::: _) : ps)
      | c == c' = Just (args ++ ps)
      | otherwise = Nothing
    s' ((W ::: _) : ps) = Just $ ((W :::) <$> ts) ++ ps
    s' [] = error "invalid matrix"

d :: M t -> M t
d = mapMaybe d'
  where
    d' ((C _ _ ::: _) : _) = Nothing
    d' ((W ::: _) : ps) = Just ps
    d' [] = error "invalid matrix"

i :: (t -> Map C [t]) -> [t] -> M t -> Maybe [PT t]
i _ [] [] = Just []
i _ [] _ = Nothing
i ctors (t' : ts) m =
  if null csd
    then asum (fmap (uncurry i') (Map.toList cs'))
    else i''
  where
    cs = Map.fromList [(c', fmap typeOf pts) | ((C c' pts ::: _) : _) <- m]
    cs' = ctors t'
    csd = Map.difference cs cs'

    i' c ts' =
      let p rps =
            let (rs, ps) = splitAt (length ts') rps
             in (C c rs ::: t') : ps
       in (p <$> i ctors (ts' ++ ts) (s c ts' m))

    i'' = p <$> i ctors ts (d m)
      where
        p rps = case Map.toList csd of
          [] -> (W ::: t') : rps
          ((c, ts') : _) -> (C c (fmap (W :::) ts') ::: t') : rps
