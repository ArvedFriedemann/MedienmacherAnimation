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
  symbols <- mapM oNew equation_symbs
  mapM_ oShow symbols
  symbols2 <- mapM oNew equation_symbs2
  case (symbols, symbols2) of
    ([a,x,b,eq,a',b',x'],[a2,x2,b2,eq2,a'2,b'2,x'2]) -> do
      --fork $ oShowWith x oScaleOut
      --fork $ oShowWith x' oScaleOut
      --fork $ oTransform x x2 1
      --oModify a (& oTopY .~ 3)
      moveAbsPerc a 1 (0.1,0.1)
      moveAbsPerc b 1 (0.2,0.2)

      fork $ oHideWith x oScaleOut
      fork $ oShowWith x2 oScaleIn
      fork $ oHideWith x' oScaleOut
      fork $ oShowWith x'2 oScaleIn
    (l,_) -> error $ "list length is " ++ show (length l)
  wait 1

moveAbsPerc :: Object s a -> Duration -> (Double, Double) -> Scene s ()
moveAbsPerc obj d (rx, ry) = oTweenS obj d $ \t -> do
  oLeftX %= \origin -> fromToS origin (screenLeft + screenWidth * rx) t
  oTopY %= \origin -> fromToS origin (screenTop - screenHeight * ry) t

equation :: SVG
equation =  translate 3 3 $ scale 2 $ center $
  latexAlign "a\\ X\\ bc = a\\ bc\\ X"

equation2 :: SVG
equation2 = scale 2 $ center $
  latexAlign "a\\ bc\\ bc = a\\ bc\\ bc"

equation_symbs :: [SVG]
equation_symbs = [snd $ splitGlyphs i equation | i <- [[0],[1],[2,3],[4],[5],[6,7],[8]]]

equation_symbs2 :: [SVG]
equation_symbs2 = [snd $ splitGlyphs i equation2 | i <- [[0],[1,2],[3,4],[5],[6],[7,8],[9,10]]]

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "lightblue"


{-
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

-}
