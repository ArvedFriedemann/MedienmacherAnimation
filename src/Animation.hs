{-# LANGUAGE OverloadedStrings #-}
module Animation where

import Reanimate
import Reanimate.Scene
import Control.Lens
import Control.Monad

animateMain :: IO ()
animateMain = putStrLn "Animating..." >> (reanimate  $ animation)
--animateMain = reanimate $ addStatic (mkBackground "cyan") $ staticFrame 1 $ scale 3 $ center $ latexAlign "E = mc^2"

animation :: Animation
animation = env $ scene $ do
  symbols <- mapM oNew
    [symb_e, symb_eq, symb_m, symb_c2]
  mapM_ oShow symbols

  forM_ (zip symbols yPositions) $
    \(obj, yPos) -> do
    fork $ oTweenS obj 1 $ \t -> do
      oScale %= \origin ->
        fromToS origin scaleFactor t
      oLeftX %= \origin ->
        fromToS origin screenLeft t
      oCenterY %= \origin ->
        fromToS origin yPos t
    wait 0.3

symb_e :: SVG
symb_e = snd $
  splitGlyphs [0] equation

symb_eq :: SVG
symb_eq = snd $
  splitGlyphs [1] equation

symb_m :: SVG
symb_m = snd $
  splitGlyphs [2] equation

symb_c2 :: SVG
symb_c2 = snd $
  splitGlyphs [3,4] equation

equation :: SVG
equation = scale 3 $ center $
  latexAlign "E = mc^2"

yPositions = [3,1,-1,-3]
scaleFactor = 0.3

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"
