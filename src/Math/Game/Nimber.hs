{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.Game.Nimber
       (
         Nimber(..)
       )
  where

import Data.Bits

-- Stolen from https://hackage.haskell.org/package/nimber
newtype Nimber = Nimber { unNimber :: Integer } deriving (Eq, Ord, Enum)

log2 :: Integer -> Integer
log2 1 = 0
log2 2 = 1
log2 x = 1 + log2 (x `div` 2)

level :: Integer -> Integer
level n = log2 $ log2 n

size :: Nimber -> Int
size = bit . fromInteger . level . unNimber

nimBit :: Int -> Nimber
nimBit = Nimber . bit

split :: Int -> Nimber -> (Nimber, Nimber)
split k = \(Nimber a) -> (Nimber (a `shiftR` k), Nimber (a .&. mask))
  where mask = complement (-1 `shiftL` k)

join :: Int -> (Nimber, Nimber) -> Nimber
join k (Nimber a1, Nimber a0) = Nimber ((a1 `shiftL` k) .|. a0)

square :: Nimber -> Nimber
square 0 = 0
square 1 = 1
square a = join k (p1, p0 + p1 * nimBit (k - 1))
  where k = size a
        (a1, a0) = split k a
        p0 = square a0
        p1 = square a1

instance Num Nimber where
  Nimber a + Nimber b = Nimber $ a `xor` b

  (-) = (+)
  negate = id

  0 * _ = 0
  _ * 0 = 0
  1 * a = a
  a * 1 = a
  a * b | a == b = square a
  a * b = join k (p0 + (a0 + a1) * (b0 + b1), p0 + p1 * nimBit (k - 1))
    where k = max (size a) (size b)
          (a1, a0) = split k a
          (b1, b0) = split k b
          p0 = a0 * b0
          p1 = a1 * b1

  fromInteger = Nimber
  signum 0 = 0
  signum _ = 1
  abs = id

instance Fractional Nimber where
  recip 0 = error "Nimber: recip 0"
  recip 1 = 1
  recip a = (a + a1) * recip b where
    k = size a
    (a1, a0) = split k a
    b = a0 * (a0 + a1) + a1 * a1 * nimBit (k - 1)

  fromRational = error "Nimber: fromRational"

instance Show Nimber where
  showsPrec d (Nimber 0) = showParen (d > 7) $ showChar '0'
  showsPrec d (Nimber 1) = showParen (d > 7) $ showChar '∗'
  showsPrec d (Nimber a) = showParen (d > 7) $ showChar '∗' . showsPrec 8 a
