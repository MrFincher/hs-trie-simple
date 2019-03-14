import qualified Data.Map.Lazy as M
import Data.Bool
import Data.Maybe
 
data Trie a = Trie { endHere :: Bool
                   , getTrie :: M.Map a (Trie a)
                   } deriving (Eq)

empty :: Trie a               
empty = Trie False M.empty

insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie _ m) = Trie True m
insert (x:xs) (Trie e m) = Trie e $ M.alter (Just . insert xs . fromMaybe empty) x m

member :: Ord a => [a] -> Trie a -> Bool
member [] (Trie e _) = e
member (x:xs) (Trie e m) = maybe False (member xs) $ M.lookup x m

countEntries :: Trie a -> Int 
countEntries (Trie e m) = bool 0 1 e + sum (map countEntries $ M.elems m)

prefixFinder :: Ord a => [a] -> Trie a -> Int
prefixFinder [] t = countEntries t
prefixFinder (x:xs) (Trie e m) = maybe 0 (prefixFinder xs) $ M.lookup x m

main = do
  let t = insert "hack" $ insert "hackerrank" empty
  print $ prefixFinder "hac" t