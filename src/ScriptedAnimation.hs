{-# LANGUAGE OverloadedStrings #-}
module ScriptedAnimation where

import Animation
import Reanimate
import Reanimate.Scene
import Control.Lens
import Control.Monad
import Data.Text (Text)


initialProblem :: Animation
initialProblem = scene $ do
  bird <- oNew $ stdLaTeX "bird\\ is\\ a\\ noun"
  pers <- oNew $ stdLaTeX "person\\ is\\ a\\ noun"
  flyi <- oNew $ stdLaTeX "flies\\ is\\ a\\ verb"
  driv <- oNew $ stdLaTeX "drives\\ is\\ a\\ verb"
  sent <- oNew $ stdLaTeX $ "&sentence\\ (the\\ X\\ Y)\\ \\vdash\\\\ &\\qquad X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  let lst = [bird,pers, flyi,driv,sent]
  let lineThicknessThin = 0.12
  let lineThicknessThick = 0.15
  forM (zip (init lst) [0..]) $ \(o,i) ->
    setTopLeftPerc (0,i*lineThicknessThin) o
  let lastPos = (0,((fromIntegral $ length lst) - 1)*lineThicknessThin + lineThicknessThick)

  setTopLeftPerc lastPos sent
  waitOn $ mapM (\x -> fork $ oShowWith x oScaleIn {-oDraw-}) lst
  mapM (fork . oHide) lst

  sent2 <- oNew $ stdLaTeX $ "sentence\\ S?"
  setTopLeftPerc lastPos sent2
  fork $ oShow sent2
  moveAbsPerc sent2 1 (0.3,0)

  oHide sent2
  (sent2r,sent2s) <- oNewTup $ splitGlyphs [8] $
    svgTranslateTopLeftPerc (0.3,0) $
    stdLaTeX $ "sentence\\ S?"
  oShow sent2r
  oShowWith sent2s wiggleAnim
  mapM oHide [sent2r,sent2s]
  oShow sent2

  sent3 <- oNew $ stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash"
  fork $ moveAbsPerc sent2 1 (0.2,0)
  setTopLeftPerc (0.2,lineThicknessThin) sent3
  oShowWith sent3 oScaleIn

  fork $ oHideWith sent2 oScaleOut
  sent4 <- oNew $ stdLaTeX $ "sentence\\ (the\\ X\\ Y)?"
  setTopLeftPerc (0.2,0) sent4
  oShowWith sent4 oScaleIn

  waitOn $ do
    fork $ moveAbsPerc sent3 1 (0,lineThicknessThin)
    fork $ moveAbsPerc sent4 1 (0,0)
  sent5 <- oNew $ stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*2) sent5
  oShowWith sent5 oScaleIn

  (sentr, sentx) <- oNewTup $ splitGlyphs [12] $
    svgTranslateTopLeftPerc (0,0) $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)?"
  (ruler, rulex) <- oNewTup $ splitGlyphs [12] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash"
  (tailr, tailx) <- oNewTup $ splitGlyphs ([0]++[4..7]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  (_, tailx) <- oNewTup $ splitGlyphs ([0]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  (_, tailnoun) <- oNewTup $ splitGlyphs ([4..7]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  mapM oHide [sent3,sent4,sent5]
  mapM oShow [sentr,sentx,ruler,rulex, tailr, tailx,tailnoun]
  waitOn $ mapM (fork . flip oShowWith wiggleAnim) [sentx,rulex,tailx,tailnoun]
  mapM oHide [sentr,sentx,ruler,rulex, tailr, tailx,tailnoun]

  (sentr2, sentx2) <- oNewTup $ splitGlyphs [13] $
    svgTranslateTopLeftPerc (0,0) $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)?"
  (ruler2, rulex2) <- oNewTup $ splitGlyphs [13] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash"
  (tailr2, tailx2) <- oNewTup $ splitGlyphs ([9]++[13..16]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  (_, tailx2) <- oNewTup $ splitGlyphs ([9]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  (_, tailnoun2) <- oNewTup $ splitGlyphs ([13..16]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  mapM oHide [sent3,sent4,sent5]
  mapM oShow [sentr2,sentx2,ruler2,rulex2, tailr2, tailx2,tailnoun2]
  waitOn $ mapM (fork . flip oShowWith wiggleAnim) [sentx2,rulex2,tailx2,tailnoun2]
  --mapM oHide [sentr2,sentx2,ruler2,rulex2, tailr2, tailx2,tailnoun2]
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*3) bird
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*4) pers
  oShow bird
  oShow pers
  wait 1
  mapM oHide [sentr2,sentx2,ruler2,rulex2, tailr2, tailx2,tailnoun2]

  (sentrb, sentxb) <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,0) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)?"
  (rulerb, rulexb) <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)\\ \\vdash"
  (tailrb, tailxb) <- oNewTup $ splitGlyphs ([0..3]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"

  mapM oShow [sentrb,sentxb,rulerb,rulexb, tailrb, tailxb]
  waitOn $ mapM (fork . flip oShowWith wiggleAnim) [sentxb,rulexb,tailxb]
  mapM (fork . flip oHideWith oFadeOut) [bird,pers]
  --mapM oHide [sentr,sentx,ruler,rulex, tailr, tailx,tailnoun]
  mapM oHide [tailrb,tailxb]

  newtail <- oNew $ stdLaTeX $ "bird\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  oShow newtail
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*2) newtail
  moveAbsPerc newtail 1 (lineThicknessThin/2,lineThicknessThin*2)

  let offset = 4
  setTopLeftPerc (lineThicknessThin*offset,lineThicknessThin*3) flyi
  setTopLeftPerc (lineThicknessThin*offset,lineThicknessThin*4) driv
  oShow flyi
  oShow driv
  wait 1

  (sentr2y, sentx2y) <- oNewTup $ splitGlyphs [16..20] $
    svgTranslateTopLeftPerc (0,0) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ flies)?"
  (ruler2y, rulex2y) <- oNewTup $ splitGlyphs [16..20] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ flies)\\ \\vdash"
  (tailr2y, tailx2y) <- oNewTup $ splitGlyphs ([12..16]++[20..23]) $
    svgTranslateTopLeftPerc (lineThicknessThin/2,lineThicknessThin*2) $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ flies\\ is\\ a\\ verb"
  (_, tailx2y) <- oNewTup $ splitGlyphs ([12..16]) $
    svgTranslateTopLeftPerc (lineThicknessThin/2,lineThicknessThin*2) $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ flies\\ is\\ a\\ verb"
  (_, tailnoun2y) <- oNewTup $ splitGlyphs ([20..23]) $
    svgTranslateTopLeftPerc (lineThicknessThin/2,lineThicknessThin*2) $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ flies\\ is\\ a\\ verb"
  mapM oHide [sentrb,sentxb,rulerb,rulexb, tailrb, tailxb, newtail]
  mapM oShow [sentr2y,sentx2y,ruler2y,rulex2y, tailr2y, tailx2y,tailnoun2y]
  waitOn $ mapM (fork . flip oShowWith wiggleAnim) [sentx2y,rulex2y,tailx2y,tailnoun2y]
  wait 1

  sent6 <- oNew $ stdLaTeX $ "sentence\\ (the\\ bird\\ flies)."
  setTopLeftPerc (0,0) sent6

  mapM (flip oHideWith oFadeOut) [driv, flyi]
  waitOn $ mapM (fork . flip oHideWith oFadeOut) [tailr2y, tailx2y,tailnoun2y]
  waitOn $ mapM (fork . flip oHideWith oFadeOut) [ruler2y,rulex2y]
  oShow sent6
  mapM oHide [sentr2y,sentx2y]
  wait 1
  moveAbsPerc sent6 1 (0.12,0.4)


  wait 10

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

oNewTup :: (SVG,SVG) -> Scene s (Object s SVG, Object s SVG)
oNewTup (a,b) = do
  ao <- oNew a
  bo <- oNew b
  return (ao,bo)
