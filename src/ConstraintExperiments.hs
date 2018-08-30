{-# LANGUAGE ConstraintKinds #-}

module ConstraintExperiments where

import Data.Constraint
import Data.Constraint.Deferrable

x :: (Eq a) => a -> Dict (Eq a)
x a = Dict

class XCtx a where
  xc :: a -> Constraint

instance XCtx Int where
  xc a = undefined

checkA :: (Eq a) => a -> a -> Bool
checkA x y = x == y

y :: Dict (Eq a) -> (a -> a -> Bool)
y d = withDict d checkA