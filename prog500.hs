import Control.Arrow
import Control.Monad.State.Strict
import qualified Data.Foldable as F
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Printf

type Card = Int

allCards :: MultiSet Card
allCards = MultiSet.fromOccurList [(if i >= 11 then 10 else i, 8) | i <- [1..13]]

val :: MultiSet Card -> Int
val xs
  | 1 `MultiSet.member` xs && s <= 11 = s + 10
  | otherwise = s
  where
    s = F.sum xs

type Hands = (MultiSet Card, MultiSet Card)

initialHands :: Hands
initialHands = (p, d)
  where
    p = MultiSet.fromList [1,2]
    d = MultiSet.fromList [3]

pickCard :: Hands -> [(Card, Double)]
pickCard (p,d) = [(c, fromIntegral n / fromIntegral size) | (c, n) <- MultiSet.toOccurList rest]
  where
    rest = allCards `MultiSet.difference` (p `MultiSet.union` d)
    size = MultiSet.size rest

type M = State (Map Hands Double, Map Hands Double)

probWin :: Hands -> M Double
probWin (p,d)
  | F.sum p > 21 = return 0
  | otherwise = liftM2 max (probWinWhenHit (p,d)) (probWinWhenStand (p,d))

probWinWhenHit :: Hands -> M Double
probWinWhenHit (p,d) = do
  cache <- gets fst
  case Map.lookup (p,d) cache of
    Just prob -> return prob
    Nothing -> do
      prob <- liftM sum $ forM (pickCard (p,d)) $ \(c,prob) -> do
        liftM (prob *) $ probWin (MultiSet.insert c p, d)
      modify $ first $ Map.insert (p,d) prob
      return prob

probWinWhenStand :: Hands -> M Double
probWinWhenStand (p,d) = do
  cache <- gets snd
  case Map.lookup (p,d) cache of
    Just prob -> return prob
    Nothing -> do
      ret <- do
        let p_val = val p
        let d_val = val d
        if F.sum p > 21 then
          return 0
        else if F.sum d > 21 then
          return 1
        else if d_val >= 17 then
          return (if p_val > d_val then 1 else 0)
        else do
          liftM sum $ forM (pickCard (p,d)) $ \(c, prob1) -> do
            prob2 <- probWinWhenStand (p, MultiSet.insert c d)
            return $ prob1 * prob2
      modify $ second $ Map.insert (p,d) $! ret
      return ret

main :: IO ()
main = do
  let (p1,p2) = evalState
        (liftM2 (,) (probWinWhenHit initialHands) (probWinWhenStand initialHands))
        (Map.empty, Map.empty)
  printf "TMCTF{HIT:%.4f:STAND:%.4f}\n" p1 p2
