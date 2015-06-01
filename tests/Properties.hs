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

instance Arbitrary Game where
  arbitrary = Game <$> arbitrary <*> arbitrary

instance Arbitrary Nimber where
  arbitrary = Nimber <$> pos
    where pos = getPositive <$> arbitrary

instance Arbitrary NumberUpStar where
  arbitrary = NUS <$> ((%) <$> n <*> d) <*> choose (-5, 5) <*> arbitrary
    where n = arbitrary
          d = (2^) <$> choose (0::Int, 5)
  shrink (NUS n u s) =  [NUS n' u' s' | (n', u', s') <- shrink (n, u, s)]


prop_nusEquals nus = nusToOptionsGame nus == NUSGame nus

main :: IO ()
main = $defaultMainGenerator
