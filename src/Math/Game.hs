module Math.Game
       (
         Game(..), NumberUpStar (..),
         game, leftMoves, rightMoves,
         star, up, down,
         (||),
         birthday,
         nusToOptionsGame,
       )
  where

import Prelude hiding ((||))
import Control.Monad ((>=>))
import Data.List
import Data.Ratio

import Math.Game.Nimber

data Game = Game [Game] [Game]
          | NUSGame NumberUpStar

game :: [Game] -> [Game] -> Game
game l r = let g = simplify (Game l r) in
           case simplifiedToNUS g of
             Just nus -> NUSGame nus
             Nothing  -> g

leftMoves :: Game -> [Game]
leftMoves (Game l _) = l
leftMoves (NUSGame (NUS 0 0 0)) = []
leftMoves (NUSGame (NUS i 0 0)) | denominator i == 1 && i > 0 = [fromInteger $ numerator i - 1]
leftMoves (NUSGame (NUS i 0 0)) | denominator i == 1 && i < 0 = []
leftMoves (NUSGame (NUS r 0 0)) = [fromRational $ (numerator r - 1) % denominator r]
leftMoves (NUSGame (NUS r 1 1)) = [NUSGame (NUS r 0 0), NUSGame (NUS r 0 1)]
leftMoves (NUSGame (NUS r (-1) 1)) = [NUSGame (NUS r 0 0)]
leftMoves (NUSGame (NUS r 0 k)) = map (NUSGame . NUS r 0 . Nimber) [0 .. unNimber k - 1]
leftMoves (NUSGame (NUS n u s)) | u > 0 = [NUSGame (NUS n 0 0)]
leftMoves (NUSGame (NUS n u s)) | u < 0 = [NUSGame (NUS n (u+1) (s+1))]

rightMoves :: Game -> [Game]
rightMoves (Game _ r) = r
rightMoves (NUSGame (NUS 0 0 0)) = []
rightMoves (NUSGame (NUS i 0 0)) | denominator i == 1 && i > 0 = []
rightMoves (NUSGame (NUS i 0 0)) | denominator i == 1 && i < 0 = [fromInteger $ numerator i + 1]
rightMoves (NUSGame (NUS r 0 0)) = [fromRational $ (numerator r + 1) % denominator r]
rightMoves (NUSGame (NUS r 1 1)) = [NUSGame (NUS r 0 0)]
rightMoves (NUSGame (NUS r (-1) 1)) = [NUSGame (NUS r 0 0), NUSGame (NUS r 0 1)]
rightMoves (NUSGame (NUS r 0 k)) = map (NUSGame . NUS r 0 . Nimber) [0 ..  unNimber k - 1]
rightMoves (NUSGame (NUS n u s)) | u > 0 = [NUSGame (NUS n (u-1) (s-1))]
rightMoves (NUSGame (NUS n u s)) | u < 0  = [NUSGame (NUS n 0 0)]

star, up, down :: Game
star = NUSGame (NUS 0   0  1)
up   = NUSGame (NUS 0   1  0)
down = NUSGame (NUS 0 (-1) 0)

instance Num Game where
  NUSGame g + NUSGame h = NUSGame $ g + h
  g + h | isNumber g = Game (map (g +) (leftMoves h)) (map (g +) (rightMoves h))
  g + h | isNumber h = Game (map (h +) (leftMoves g)) (map (h +) (rightMoves g))
  g + h = game left right
    where left  = map (+ h) (leftMoves g)  ++ map (g +) (leftMoves h)
          right = map (+ h) (rightMoves g) ++ map (g +) (rightMoves h)

  negate (Game l r) = Game (map negate r) (map negate l)
  negate (NUSGame nus) = NUSGame $ negate nus

  fromInteger = NUSGame . fromInteger

  _ * _ = undefined -- This is possible to define for some games
  abs _ = undefined
  signum _ = undefined

instance Fractional Game where
  fromRational r = NUSGame (NUS r 0 0)

instance Eq Game where
  NUSGame g == NUSGame h = g == h
  g == h = (g <= h) && (h <= g)

instance Ord Game where
  NUSGame g <= NUSGame h = g <= h
  g <= h = not (any (h <=) (leftMoves g)) && not (any (<= g) (rightMoves h))

(||) :: Game -> Game -> Bool
g || h = not (g <= h) && not (h <= g)

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

bypassReversible :: Game -> Game
bypassReversible g = Game left right
  where left  = concatMap (lReversible g) (leftMoves g)
        right = concatMap (rReversible g) (rightMoves g)

simplify :: Game -> Game
simplify = removeDominated . bypassReversible

data NumberUpStar = NUS { numberPart :: Rational, upPart :: Integer, nimberPart :: Nimber } deriving (Eq)

instance Num NumberUpStar where
  (NUS n u s) + (NUS n' u' s') = NUS (n + n') (u + u') (s + s')
  negate (NUS n u s) = NUS (-n) (-u) s

  fromInteger i = NUS (fromInteger i) 0 0

  _ * _ = undefined
  abs _ = undefined
  signum _ = undefined

instance Ord NumberUpStar where
  g <= h | g == h                      = True
         | numberPart g < numberPart h = True
         | numberPart g > numberPart h = False
         | upPart g     < upPart h - 1 = True
         | upPart g    == upPart h - 1 = nimberPart g + nimberPart h /= 1
         | otherwise                   = False

nusIsNumber :: NumberUpStar -> Bool
nusIsNumber nus = upPart nus == 0 && nimberPart nus == 0

isNumber :: Game -> Bool
isNumber (Game _ _) = False -- TODO
isNumber (NUSGame nus) = nusIsNumber nus

log2 :: Integer -> Integer
log2 1 = 0
log2 2 = 1
log2 x = 1 + log2 (x `div` 2)

denominatorExp :: Rational -> Integer
denominatorExp = log2 . denominator

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

-- This is a prism
simplifiedToNUS :: Game -> Maybe NumberUpStar
simplifiedToNUS = nusOptionsFrom >=> optionsToNUS

nusToOptionsGame :: NumberUpStar -> Game
nusToOptionsGame nus = Game (leftMoves nusGame) (rightMoves nusGame)
  where nusGame = NUSGame nus

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
                       | otherwise = show n

instance Show Game where
  show (NUSGame nus) = show nus
  show g = "{ " ++ leftString ++ " | " ++ rightString ++ " }"
    where leftString = intercalate ", " (map show (leftMoves g))
          rightString = intercalate ", " (map show (rightMoves g))

birthday :: Game -> Integer
birthday (NUSGame (NUS n u s)) = numberBirthday n + upStarBirthday u s
  where numberBirthday n | denominator n == 1 = numerator n
        numberBirthday n = 1 + truncate n + denominatorExp n
        upStarBirthday 0 0 = 0
        upStarBirthday u 0 = abs u + 1
        upStarBirthday u s | odd u && s /= 1 = abs u + unNimber (s + 1)
        upStarBirthday u s = abs u + unNimber s
birthday (Game [] []) = 0
birthday (Game l r) = 1 + maximum (map birthday (l ++ r))

-- temperature :: Game -> Rational
-- cool :: Game -> Rational -> Game
-- freeze :: Game -> Game
-- heat :: Game -> Game -> Game
-- leftStop :: Game -> Rational
-- rightStop :: Game -> Rational
-- allSmall :: Game -> Bool
-- atomicWeight :: Game -> Rational
-- leftIncentives :: Game -> [Game]
-- rightIncentives :: Game -> [Game]
-- nortonMultiply :: Game -> Game -> Game
-- conwayMultiply :: Game -> Game -> Game
-- ordinalSum :: Game -> Game -> Game
-- mean :: Game -> Rational
-- thermograph :: Game -> Thermograph
