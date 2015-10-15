
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Foldable as F
import Debug.Trace

type State = Map Char Char

swap :: Char -> Char -> Map Char Char -> Map Char Char
swap c1 c2 s = Map.insert c1 (s Map.! c2) $ Map.insert c2 (s Map.! c1) $ s


startNode :: Char
startNode = 'P'

connections :: Map Char [Char]
connections = Map.fromList tbl1 `Map.union` Map.fromListWith (++) tbl2
  where
    tbl1 = [('A', "IJ"), ('B', "IJK"), ('C',"JKL"), ('D',"KLM"), ('E',"LMN"), ('F', "MNO"), ('G', "NOP"), ('H',"OP")]
    tbl2 = [(c2, [c1]) | (c1,cs) <- tbl1, c2 <- cs]

{-
startNode :: Char
startNode = 'F'

connections :: Map Char [Char]
connections = Map.fromList tbl1 `Map.union` Map.fromListWith (++) tbl2
  where
    tbl1 = [('A', "DE"), ('B', "DEF"), ('C',"EF")]
    tbl2 = [(c2, [c1]) | (c1,cs) <- tbl1, c2 <- cs]
-}

start :: State
start = Map.fromList [(c,c) | c <- ['A'..startNode]]

goal :: State
goal = swap 'A' startNode start

move :: (State, Seq Char) -> [(State, Seq Char)]
move (s, hist) = do
  let ppos = head [pos | (pos, c) <- Map.toList s, c == startNode]
  pos2 <- connections Map.! ppos
  return ((swap ppos pos2 s), hist |> pos2)

solve = loop [(start, Seq.empty)] Set.empty 0 
  where
    loop :: [(State, Seq Char)] -> Set State -> Int -> Seq Char
    loop ss visited level
      | traceShow (level, length ss, Set.size visited) False = undefined
      | hists <- [hist | (s, hist) <- ss, s == goal], not (null hists) = minimum hists
      | otherwise = loop (Map.toList ss') (visited `Set.union` Map.keysSet ss') (level + 1)
          where
            ss' = Map.fromListWith min [(s2,hist2) | (s1,hist1) <- ss, (s2,hist2) <- move (s1,hist1), s2 `Set.notMember` visited]

main = do
  print connections
  putStrLn $ F.toList $ solve

