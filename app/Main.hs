module Main where

import Reanimate
import Animation
import ScriptedAnimation

main :: IO ()
--main = animateMain
--main = reanimate $ env $ initialProblem
--main = reanimate $ env $ interlude
main = reanimate $ env $ checkExample
