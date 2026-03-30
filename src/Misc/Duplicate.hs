module Misc.Duplicate (sepUniqDup, sepUniqDupBy) where

import qualified Data.Set as Set

sepUniqDup :: (Ord k) => [(k, v)] -> ([(k, v)], [(k, v)])
sepUniqDup kvs = (uniques', duplicates')
  where
    (uniques', duplicates', _) = foldl go ([], [], Set.empty) kvs
    go (uniques, duplicates, seen) (k, v)
      | Set.member k seen = (uniques, duplicates ++ [(k, v)], seen)
      | otherwise = (uniques ++ [(k, v)], duplicates, Set.insert k seen)

sepUniqDupBy :: (Ord k) => (v -> k) -> [v] -> ([v], [v])
sepUniqDupBy f xs =
  let (uniqs, dups) = sepUniqDup $ fmap (\x -> (f x, x)) xs
   in (fmap snd uniqs, fmap snd dups)
