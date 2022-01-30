{-# LANGUAGE OverloadedStrings #-}
module Animation where

import Reanimate
import Reanimate.Scene
import Control.Lens
import Control.Monad
import Data.Text (Text)

lineThicknessThin = 0.12
lineThicknessThick = 0.15

animateMain :: IO ()
animateMain = putStrLn "Animating..." >> (reanimate  $ animation)
--animateMain = reanimate $ addStatic (mkBackground "cyan") $ staticFrame 1 $ scale 3 $ center $ latexAlign "E = mc^2"

animation :: Animation
animation = env $ scene $ do
  symbols <- mapM oNew equation_symbs
  mapM_ oShow symbols
  symbols2 <- mapM oNew equation_symbs2
  mapM (flip oShowWith wiggleAnim) symbols
  case (symbols, symbols2) of
    ([a,x,b,eq,a',b',x'],[a2,x2,b2,eq2,a'2,b'2,x'2]) -> do
      --fork $ oShowWith x oScaleOut
      --fork $ oShowWith x' oScaleOut
      --fork $ oTransform x x2 1
      --oModify a (& oTopY .~ 3)
      moveAbsPerc a 1 (0.1,0.1)
      moveAbsPerc b 1 (0.2,0.2)

      fork $ oHideWith x oScaleOut
      fork $ oShowWith x2 oDraw
      fork $ oHideWith x' oScaleOut
      fork $ oShowWith x'2 oDraw
    (l,_) -> error $ "list length is " ++ show (length l)
  wait 1

moveAbsPerc :: Object s a -> Duration -> (Double, Double) -> Scene s ()
moveAbsPerc obj d (rx, ry) = oTweenS obj d $ \t -> do
  oLeftX %= \origin -> fromToS origin (screenLeft + screenWidth * rx) t
  oTopY %= \origin -> fromToS origin (screenTop - screenHeight * ry) t

moveAbsPercY :: Object s a -> Duration -> Double -> Scene s ()
moveAbsPercY obj d ry = oTweenS obj d $ \t -> do
  oTopY %= \origin -> fromToS origin (screenTop - screenHeight * ry) t

moveCenterAbsPerc :: Object s a -> Duration -> (Double, Double) -> Scene s ()
moveCenterAbsPerc obj d (rx, ry) = oTweenS obj d $ \t -> do
  oCenterX %= \origin -> fromToS origin (screenLeft + screenWidth * rx) t
  oCenterY %= \origin -> fromToS origin (screenTop - screenHeight * ry) t

moveCenterAbsPercX :: Object s a -> Duration -> Double -> Scene s ()
moveCenterAbsPercX obj d rx = oTweenS obj d $ \t -> do
  oCenterX %= \origin -> fromToS origin (screenLeft + screenWidth * rx) t

--Taken from example
highlightE :: Effect
highlightE d t =
  scale (1 + bellS 2 (t/d)*0.5) . rotate (wiggleS (t/d) * 20)

--Taken from example
-- s-curve, sin, s-curve
wiggleS :: Signal
wiggleS t
  | t < 0.25  = curveS 2 (t*4)
  | t < 0.75  = sin ((t-0.25)*2*pi+pi/2)
  | otherwise = curveS 2 ((t-0.75)*4)-1

wiggle :: Sprite s -> Scene s ()
wiggle s = spriteE s $ overBeginning 1 $ aroundCenterE highlightE

wiggleAnim :: SVG -> Animation
wiggleAnim svg = animate $ \t -> (overBeginning 1 $ aroundCenterE highlightE) 1 t svg

equation :: SVG
equation =  translate 3 3 $ stdLaTeX "a\\ X\\ bc = a\\ bc\\ X"

equation2 :: SVG
equation2 = stdLaTeX "a\\ bc\\ bc = a\\ bc\\ bc"

whiteFill :: SVG -> SVG
whiteFill = withFillColor "white"

whiteOutl :: SVG -> SVG
whiteOutl = withStrokeColor "white"

white :: SVG -> SVG
white = whiteFill . whiteOutl

withColor :: String -> SVG -> SVG
withColor c = withFillColor c . withStrokeColor c

--doesn't work
oColor :: String -> SVG -> Animation
oColor c svg = animate $ const $ withColor c svg

stdLaTeX :: Text -> SVG
stdLaTeX = withStrokeWidth 0.001 . white . center . latexAlign

equation_symbs :: [SVG]
equation_symbs = [snd $ splitGlyphs i equation | i <- [[0],[1],[2,3],[4],[5],[6,7],[8]]]

equation_symbs2 :: [SVG]
equation_symbs2 = [snd $ splitGlyphs i equation2 | i <- [[0],[1,2],[3,4],[5],[6],[7,8],[9,10]]]

env :: Animation -> Animation
env = addStatic bg

bg :: SVG
bg = mkBackground "black"

--magic 0.5 is the standart margin. UNSAFE! TODO
svgTranslateTopLeftPerc :: (Double, Double) -> SVG -> SVG
svgTranslateTopLeftPerc (rx, ry) svg =
  translate (-minx-(screenWidth/2)+(screenWidth*rx)+0.5) (-miny+(screenHeight/2)-(screenHeight*ry)-height-0.5) svg
  where
    (minx,miny,width,height) = boundingBox svg
    (x,y) = relCoords (rx, ry)

relCoords :: (Double, Double) -> (Double, Double)
relCoords (x,y) = (screenLeft + screenWidth * x, screenTop - screenHeight * y)

setTopLeftPerc :: (Double, Double) -> Object s a -> Scene s ()
setTopLeftPerc (rx,ry) obj = oModifyS obj $ do
  let (x,y) = relCoords (rx,ry)
  oLeftX .= x
  oTopY .= y

setTopLeftPercY :: Double -> Object s a -> Scene s ()
setTopLeftPercY ry obj = oModifyS obj $ do
  let (x,y) = relCoords (0,ry)
  oTopY .= y

setCenterPerc :: (Double,Double) -> Object s a -> Scene s ()
setCenterPerc (rx,ry) obj = oModifyS obj $ do
  let (x,y) = relCoords (rx,ry)
  oCenterX .= x
  oCenterY .= y

setCenterPercX :: Double -> Object s a -> Scene s ()
setCenterPercX rx obj = oModifyS obj $ do
  let (x,y) = relCoords (rx,0)
  oCenterX .= x

oNewTup :: (SVG,SVG) -> Scene s (Object s SVG, Object s SVG)
oNewTup (a,b) = do
  ao <- oNew a
  bo <- oNew b
  return (ao,bo)

waitSafeUntil :: Time -> Scene s ()
waitSafeUntil tNew = do
  now <- queryNow
  let t = tNew - now
  if t >= 0
  then wait t
  else error $ "Missed time stamp at "++(show tNew)++" (missed by "++show t++" seconds)"

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
