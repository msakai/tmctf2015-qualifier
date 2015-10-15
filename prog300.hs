import Control.Monad
import Data.Array
import Data.Monoid
import Data.Ord
import qualified Data.Heap as H
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO
import Text.Printf

main = do
  boards <- loadProblems
  results <- forM (zip [1..] boards) $ \(n, board) -> do
    printf "Problem %d\n" (n::Int)
    hFlush stdout
    let (ans,cost) = solve board
    putStrLn ans
    print cost
    hFlush stdout
    return (ans, cost)
  putStrLn "final result"
  mapM_ (putStrLn . fst) results
  print $ sum $ map snd results

loadProblems :: IO [Board]
loadProblems = do
  f <- openFile "maze.txt" ReadMode
  replicateM 1001 $ do
    [w,h,ncheckpoints] <- liftM (map read . words) $ hGetLine f
    ls <- replicateM h (hGetLine f)
    let board = array ((0,0), (w-1,h-1)) [((x,y),c) | (y,l) <- zip [0..] ls, (x,c) <- zip [0..] l]
    return board

type Board = Array Pos Char
type Pos = (Int, Int)
type Cost = Int

data State
  = State
  { stPos  :: !Pos
  , stVisited :: !(Set Pos)
  , stUnvisitedC :: !(Set Pos)
  , stUnvisitedE :: !(Set Pos)

  , stGoal :: !Pos
  , stCost :: !Cost
  , stHistory :: String
  }

isBetterOrEqual :: State -> State -> Bool
isBetterOrEqual s1 s2 =
  stPos s1 == stPos s2 &&
  stCost s1 <= stCost s2 &&
  stVisited s1 `Set.isSubsetOf` stVisited s2 &&
  stUnvisitedC s1 `Set.isSubsetOf` stUnvisitedC s2 && 
  stUnvisitedE s1 `Set.isSubsetOf` stUnvisitedE s2

type Table = Map Pos [State]

insertTable :: State -> Table -> Table
insertTable s = Map.alter f (stPos s)
  where
    f Nothing = Just [s]
    f (Just ss2) = Just (s : [s2 | s2 <- ss2, not (s `isBetterOrEqual` s2)])

newtype HItem = HItem State

instance Ord HItem where
  compare = comparing f
    where
      f (HItem s) = stCost s * 10 + d (stPos s) (stGoal s) + sum [d (stPos s) p | p <- Set.toList (stUnvisitedC s)]
      d (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

instance Eq HItem where
  h1 == h2 = compare h1 h2 == EQ
                          
solve :: Board -> (String, Int)
solve board = minimumBy (comparing snd) [(reverse (stHistory s), stCost s) | s <- Map.findWithDefault [] goal table, Set.null (stUnvisitedC s)]
  where
    start = head [p | (p,c) <- assocs board, c == 'S']
    goal  = head [p | (p,c) <- assocs board, c == 'G']

    isValid s = stPos s == stGoal s && Set.null (stUnvisitedC s)

    table :: Table
    table = explore (H.singleton (HItem s0)) Map.empty

    explore :: H.Heap HItem -> Table -> Table
    explore ss' result =
      case H.viewMin ss' of
        Nothing -> result
        Just (HItem s, ss) ->
          case Map.lookup (stPos s) result of
            -- _ | isValid s -> explore ss (insertTable s result)
            Just ss2 | any (\s2 -> s2 `isBetterOrEqual` s) ss2 -> explore ss result
            _ -> explore (H.fromList [HItem s' | s' <- move s] <> ss) (insertTable s result)

    s0 :: State
    s0 =
      State
      { stPos  = start
      , stVisited = Set.singleton start
      , stUnvisitedC = Set.fromList [p | (p,c) <- assocs board, c == 'C']
      , stUnvisitedE = Set.fromList [p | (p,c) <- assocs board, c == 'E']

      , stGoal = goal
      , stCost = 0
      , stHistory = []
      }

    move :: State -> [State]
    move s@State{ stPos = (x,y), stVisited = visited, stUnvisitedC = unvisitedC, stUnvisitedE = unvisitedE, stCost = cost, stHistory = hist } = do
      m <- "DULR"
      let pos' =
            case m of
              'D' -> (x,y+1)
              'U' -> (x,y-1)
              'R' -> (x+1,y)
              'L' -> (x-1,y)
      guard $ inRange (bounds board) pos'
      guard $ board ! pos' /= '#'
      return $
        State
        { stPos = pos'
        , stVisited = Set.insert pos' visited
        , stUnvisitedC = Set.delete pos' unvisitedC
        , stUnvisitedE = Set.delete pos' unvisitedE

        , stGoal = stGoal s
        , stCost = cost + (if pos' `Set.member` visited then 1 else 0) + (if board ! pos' == 'E' && pos' `Set.member` unvisitedE then -20 else 0)
        , stHistory = m : hist
        }
