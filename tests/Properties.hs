{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Data.Ratio

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.Framework.TH
import Test.Framework.Providers.QuickCheck2

import Math.Game
import Math.Game.Nimber

genGame :: Int -> Gen Game
genGame n = do
    t <- arbitrary
    if t then
      nusGame <$> arbitrary
    else do
      lSize <- choose (0,n)
      rSize <- choose (0,n)

      game <$> vectorOf lSize (genGame (n-1)) <*> vectorOf rSize (genGame (n-1))

instance Arbitrary Game where
  arbitrary = genGame 4

instance Arbitrary Nimber where
  arbitrary = Nimber <$> pos
    where pos = getPositive <$> arbitrary

instance Arbitrary NumberUpStar where
  arbitrary = NUS <$> ((%) <$> n <*> d) <*> choose (-5, 5) <*> arbitrary
    where n = arbitrary
          d = (2^) <$> choose (0::Int, 5)
  shrink (NUS n u s) =  [NUS n' u' s' | (n', u', s') <- shrink (n, u, s)]


prop_nusEquals nus    = nusToOptionsGame nus == nusGame nus
prop_nusLeq a b       = a         <=         b ==> nusToOptionsGame a <= nusToOptionsGame b
prop_nusLeqLeft a b   = nusGame a <=         b ==> nusToOptionsGame a <= b
prop_nusLeqRight a b  = a         <= nusGame b ==>                  a <= nusToOptionsGame b

prop_nusBirthday nus  = birthday (nusGame nus) == birthday (nusToOptionsGame nus)

main :: IO ()
main = $defaultMainGenerator
