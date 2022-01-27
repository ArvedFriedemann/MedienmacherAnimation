module Main where

import Reanimate
import Animation
import ScriptedAnimation

main :: IO ()
--main = animateMain
main = reanimate $ env $ initialProblem
