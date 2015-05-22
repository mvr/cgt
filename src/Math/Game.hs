module Math.Game
       (
         Game (..),
         star, up, down,
         identical,
         simplify,
         (||),
       )
  where

import Prelude hiding ((||))
import qualified Prelude ((||))
import Control.Monad ((>=>))
import Data.List
import Data.Ratio

import Math.Game.Nimber

data Game = Game { leftMoves :: [Game], rightMoves :: [Game] }

star, up, down :: Game
star = Game [0] [0]
up = Game [0] [star]
down = Game [star] [0]

instance Num Game where
  g + h = Game left right
    where left  = map (+ h) (leftMoves g)  ++ map (g +) (leftMoves h)
          right = map (+ h) (rightMoves g) ++ map (g +) (rightMoves h)

  negate g = Game (map negate $ rightMoves g) (map negate $ leftMoves g)

  fromInteger i | i == 0 = Game [] []
                | i > 0  = Game [fromInteger (i-1)] []
                | i < 0  = negate $ fromInteger (-i)

  _ * _ = undefined -- This is possible to define for some games
  abs _ = undefined
  signum _ = undefined

gteqZero, lteqZero :: Game -> Bool
gteqZero = not . any lteqZero . rightMoves
lteqZero = not . any gteqZero . leftMoves

eqZero, gtZero, ltZero, fuzzyZero :: Game -> Bool
eqZero g = gteqZero g && lteqZero g
gtZero g = gteqZero g && not (lteqZero g)
ltZero g = not (gteqZero g) && lteqZero g

fuzzyZero g = not (gteqZero g) && not (lteqZero g)

instance Eq Game where
  g == h = eqZero (g - h)

instance Ord Game where
  g < h = ltZero (g - h)
  g > h = gtZero (g - h)
  g <= h = lteqZero (g - h)
  g >= h = gteqZero (g - h)

(||) :: Game -> Game -> Bool
g || h = fuzzyZero (g - h)

identical :: Game -> Game -> Bool
identical a b = inside a b && inside b a
  where inside a b =  all (\g -> any (identical g) (leftMoves b)) (leftMoves a)
                   && all (\g -> any (identical g) (rightMoves b)) (rightMoves a)

unbeaten :: (a -> a -> Bool) -> [a] -> [a]
unbeaten _ [] = []
unbeaten p (x:xs) = if any (p x) rest then
                       rest
                    else
                      x : filter (not . flip p x) rest
  where rest = unbeaten p xs

removeDominated :: Game -> Game
removeDominated g = Game left right
  where left  = unbeaten (<=) (leftMoves g)
        right = unbeaten (>=) (rightMoves g)

lReversible :: Game -> Game -> [Game]
lReversible g gl = maybe [gl] leftMoves (find (<= g) (rightMoves gl))

rReversible :: Game -> Game -> [Game]
rReversible g gr = maybe [gr] rightMoves (find (>= g) (leftMoves gr))

anyReversible :: Game -> Bool
anyReversible g = any (<= g) (concatMap rightMoves (leftMoves g)) Prelude.|| any (>= g) (concatMap leftMoves (rightMoves g))

bypassReversible :: Game -> Game
bypassReversible g = Game left right
  where left  = concatMap (lReversible g) (leftMoves g)
        right = concatMap (rReversible g) (rightMoves g)

simplifyTop :: Game -> Game
simplifyTop = removeDominated . until (not . anyReversible) (bypassReversible . removeDominated)

simplify :: Game -> Game
simplify g = simplifyTop $ Game (map simplify (leftMoves g)) (map simplify (rightMoves g))

data NumberUpStar = NUS { numberPart :: Rational, upPart :: Integer, nimberPart :: Nimber } deriving (Eq, Ord)

nusIsNumber :: NumberUpStar -> Bool
nusIsNumber nus = upPart nus == 0 && nimberPart nus == 0

optionsToNUS :: ([NumberUpStar], [NumberUpStar]) -> Maybe NumberUpStar
-- Zero game
optionsToNUS ([], []) = Just $ NUS 0 0 0
-- If G = { L | } then L is an integer and G = L + 1
optionsToNUS ([l], []) = Just $ NUS (lValue + 1) 0 0
  where lValue = numberPart l
-- If G = { | R } then R is an integer and G = R - 1
optionsToNUS ([], [r]) = Just $ NUS (rValue - 1) 0 0
  where rValue = numberPart r
        -- If G = { L | R } and L and R are both numbers, G = average of L and R
optionsToNUS ([l], [r]) | nusIsNumber l && nusIsNumber r
                          && numberPart l < numberPart r = Just $ NUS value 0 0
  where value = (numberPart l + numberPart r) / 2
-- Here we are of the form n + { 0 | G }, where G has non-negative ups
optionsToNUS ([l], [r]) | nusIsNumber l && not (nusIsNumber r)
                          && numberPart l == numberPart r
                          && upPart r >= 0
                            = Just $ NUS (numberPart l) (upPart r + 1) (nimberPart r + 1)
-- Now n + { G | 0 }, where G has non-positive ups
optionsToNUS ([l], [r]) | not (nusIsNumber l) && nusIsNumber r
                          && numberPart l == numberPart r
                          && upPart l <= 0
                            = Just $ NUS (numberPart r) (upPart l - 1) (nimberPart l + 1)
-- If G = { n, n∗ | n }, G = n↑∗
optionsToNUS ([l1, l2], [r]) | nusIsNumber l1 && nusIsNumber r
                               && l1 == r && l2 == NUS (numberPart l1) 0 1
                                 = Just $ NUS (numberPart l1) 1 1
-- If G = { n | n, n∗ }, G = n↓∗
optionsToNUS ([l], [r1, r2]) | nusIsNumber l && nusIsNumber r1
                               && l == r1 && r2 == NUS (numberPart r1) 0 1
                                 = Just $ NUS (numberPart r1) (-1) 1
-- Last possibility to check, we are looking at G = n + ∗k
optionsToNUS (l1:ls, r1:rs) | length ls == length rs && nusIsNumber l1
                              && l1 == r1 && nimberOptions = Just $ NUS (numberPart l1) 0 (fromIntegral $ length ls + 1)
  where nimberOptions = all valid (zip3 ls rs [1..])
        valid (l, r, i) = l == r && numberPart l == numberPart l1
                          && upPart l == 0 && nimberPart l == i
optionsToNUS _ = Nothing

nusOptionsFrom :: Game -> Maybe ([NumberUpStar], [NumberUpStar])
nusOptionsFrom g = do
  left <- mapM simplifiedToNUS (leftMoves g)
  right <- mapM simplifiedToNUS (rightMoves g)
  return (sort left, sort right)

-- Assumes the game given to it is simplified
simplifiedToNUS :: Game -> Maybe NumberUpStar
simplifiedToNUS = nusOptionsFrom >=> optionsToNUS

instance Show NumberUpStar where
  show (NUS 0 0 0) = "0"
  show (NUS number up star) = numberShow number ++ upShow up ++ starShow star
    where numberShow n | n == 0 = ""
                       | denominator n == 1 = show $ numerator n
                       | otherwise = "(" ++ show (numerator n) ++ "/" ++ show (denominator n) ++ ")"

          upShow n     | n == 0 = ""
                       | n > 0 = replicate (fromIntegral n) '↑'
                       | n < 0 = replicate (fromIntegral (-n)) '↓'

          starShow n   | n == 0 = ""
                       | n == 1 = "∗"
                       | otherwise = "∗" ++ show n

instance Show Game where
  show g = string (simplifiedToNUS s)
    where s = simplify g
          string (Just nus) = show nus
          string Nothing = "{ " ++ leftString ++ " | " ++ rightString ++ " }"
          leftString = intercalate ", " (map show (leftMoves s))
          rightString = intercalate ", " (map show (rightMoves s))
