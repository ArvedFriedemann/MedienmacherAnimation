module Main where

import Reanimate
import Animation
import ScriptedAnimation

main :: IO ()
--main = animateMain
--main = reanimate $ env $ initialProblem
--main = reanimate $ env $ interlude
--main = reanimate $ env $ checkExample
--main = reanimate $ env $ completionExample
--main = reanimate $ env $ intro
main = reanimate $ env $ (foldr1 seqA [title,intro,initialProblem,interlude,checkExample,completionExample])
