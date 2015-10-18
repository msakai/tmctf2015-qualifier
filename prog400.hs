{-# LANGUAGE BangPatterns #-}
import Data.Word
import Data.Array.Unboxed
import qualified Data.Foldable as F
import Data.Monoid
import Data.Ord (comparing)
import Data.Sequence (Seq, (|>), (<|), (><))
import qualified Data.Sequence as Seq
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

type State = UArray Char Word8

swap :: Char -> Char -> State -> State
swap c1 c2 s = s // [(c1, s ! c2), (c2, s ! c1)]

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
start = array ('A', startNode) [(c, fromIntegral (fromEnum c)) | c <- ['A'..startNode]]

goal :: State
goal = swap 'A' startNode start

move1 :: (State, Seq Char) -> [(State, Seq Char)]
move1 (s, hist) = do
  let ppos = head [pos | (pos, c) <- assocs s, c == fromIntegral (fromEnum (startNode))]
  pos2 <- connections Map.! ppos
  return ((swap ppos pos2 s), hist |> pos2)

move2 :: (State, Seq Char) -> [(State, Seq Char)]
move2 (s, hist) = do
  let ppos = head [pos | (pos, c) <- assocs s, c == fromIntegral (fromEnum (startNode))]
  pos2 <- connections Map.! ppos
  return ((swap ppos pos2 s), ppos <| hist)

solve :: String
solve = F.toList $ loop ([(start, Seq.empty)], Map.empty) ([(goal, Seq.empty)], Map.empty) 0 
  where
    loop :: ([(State, Seq Char)], Map State (Seq Char)) -> ([(State, Seq Char)], Map State (Seq Char)) -> Int -> Seq Char
    loop (ss1, visited1) (ss2, visited2) !level
      | traceShow (level, (length ss1, Map.size visited1), (length ss2, Map.size visited2)) False = undefined
      | hists <- Map.intersectionWith (><) visited1 visited2, not (Map.null hists) = F.minimumBy (comparing length <> compare) $ hists
      | otherwise = loop (Map.toList ss1', visited1 `Map.union` ss1') (Map.toList ss2', visited2 `Map.union` ss2') (level + 1)
          where
            ss1' = Map.fromListWith min [(s2,hist2) | (s1,hist1) <- ss1, (s2,hist2) <- move1 (s1,hist1)] `Map.difference` visited1
            ss2' = Map.fromListWith min [(s2,hist2) | (s1,hist1) <- ss2, (s2,hist2) <- move2 (s1,hist1)] `Map.difference` visited2

main :: IO ()
main = do
  print connections
  putStrLn $ "TMCTF{" ++ solve ++ "}" -- TMCTF{HOFMDKCJAIBKCJBKDMELCJBKDMELDMFOGNELDMFOGNFOHPGNFMDKBIA}
