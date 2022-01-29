{-# LANGUAGE OverloadedStrings #-}
module ScriptedAnimation where

import Animation
import Reanimate
import Reanimate.Scene
import Control.Lens
import Control.Monad
import Data.Text (Text)
import Graphics.SvgTree
import System.IO.Unsafe
import Data.Maybe
import Debug.Trace

markingColor = "yellow"

initialProblem :: Animation
initialProblem = scene $ do
  bird <- oNew $ stdLaTeX "bird\\ is\\ a\\ noun"
  pers <- oNew $ stdLaTeX "person\\ is\\ a\\ noun"
  flyi <- oNew $ stdLaTeX "flies\\ is\\ a\\ verb"
  driv <- oNew $ stdLaTeX "drives\\ is\\ a\\ verb"
  sent <- oNew $ stdLaTeX $ "&sentence\\ (the\\ X\\ Y)\\ \\vdash\\\\ &\\qquad X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  let lst = [bird,pers, flyi,driv,sent]
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


  (sentrb, sentxb) <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,0) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)?"
  (rulerb, rulexb) <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)\\ \\vdash"
  (tailrb, tailxb) <- oNewTup $ splitGlyphs ([0..3]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"

  oShowWith bird (oColor markingColor)
  mapM oHide [sentr2,sentx2,ruler2,rulex2, tailr2, tailx2,tailnoun2]
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

  oShowWith flyi (oColor markingColor)
  mapM oHide [sentrb,sentxb,rulerb,rulexb, tailrb, tailxb, newtail]
  mapM oShow [sentr2y,sentx2y,ruler2y,rulex2y, tailr2y, tailx2y,tailnoun2y]
  waitOn $ mapM (fork . flip oShowWith wiggleAnim) [sentx2y,rulex2y,tailx2y,tailnoun2y]
  wait 1

  sent6 <- oNew $ stdLaTeX $ "sentence\\ (the\\ bird\\ flies)."
  setTopLeftPerc (0,0) sent6

  waitOn $ mapM (fork . flip oHideWith oFadeOut) [driv, flyi]
  waitOn $ mapM (fork . flip oHideWith oFadeOut) [ruler2y,rulex2y,tailr2y, tailx2y,tailnoun2y]
  oShow sent6
  mapM oHide [sentr2y,sentx2y]
  wait 1
  moveCenterAbsPerc sent6 1 (0.5,0.5)


interlude :: Animation
interlude = scene $ do
  sent1 <- oNew $ stdLaTeX $ "sentence\\ (the\\ bird\\ flies)."
  setCenterPerc (0.5,0.5) sent1
  oShow sent1

  moveCenterAbsPerc sent1 1 (0.5,0.5-lineThicknessThin)

  sent2 <- oNew $ stdLaTeX $ "sentence\\ (the\\ person\\ drives)."
  setCenterPerc (0.5,0.5) sent2
  oShowWith sent2 oFadeIn

  waitOn $ do
    fork $ moveCenterAbsPerc sent2 1 (0.5,0.5-lineThicknessThin)
    fork $ moveCenterAbsPerc sent1 1 (0.5,0.5-lineThicknessThin*2)

  sent3 <- oNew $ stdLaTeX $ "sentence\\ (the\\ person\\ flies)."
  setCenterPerc (0.5,0.5) sent3
  oShowWith sent3 oFadeIn

  waitOn $ do
    fork $ moveCenterAbsPerc sent3 1 (0.5,0.5-lineThicknessThin)
    fork $ moveCenterAbsPerc sent2 1 (0.5,0.5-lineThicknessThin*2)
    fork $ moveCenterAbsPerc sent1 1 (0.5,0.5-lineThicknessThin*3)

  sent4 <- oNew $ withColor "salmon" $ stdLaTeX $ "sentence\\ (the\\ bird\\ drives)."
  setCenterPerc (0.5,0.5) sent4
  oShowWith sent4 oFadeIn
  waitOn $ mapM (fork . flip oHideWith oFadeOut) [sent1,sent2,sent3]
  oHideWith sent4 (reverseA . oDraw)

checkExample :: Animation
checkExample = scene $ do
  sent <- oNew $ stdLaTeX $ "sentence\\ (bird\\ the\\ flies)?"
  oShowWith sent oDraw
  moveAbsPerc sent 1 (0.1,0)
  oHide sent

  sentr <- oNew $
    fst $ splitGlyphs [9..12] $
    svgTranslateTopLeftPerc (0.1,0) $
    stdLaTeX $ "sentence\\ (bird\\ the\\ flies)?"
  sentb <- oNew $
    snd $ splitGlyphs [9..12] $
    svgTranslateTopLeftPerc (0.1,0) $
    stdLaTeX $ "sentence\\ (bird\\ the\\ flies)?"
  sentb' <- oNew $
    snd $ splitGlyphs [9..12] $
    svgTranslateTopLeftPerc (0.1,0) $
    withColor "salmon" $
    stdLaTeX $ "sentence\\ (bird\\ the\\ flies)?"
  mapM oShow [sentr,sentb]

  ruler <- oNew $
    fst $ splitGlyphs [9..11] $
    svgTranslateTopLeftPerc (0.1,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash..."
  ruleb <- oNew $
    snd $ splitGlyphs [9..11] $
    svgTranslateTopLeftPerc (0.1,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash..."
  ruleb' <- oNew $
    snd $ splitGlyphs [9..11] $
    svgTranslateTopLeftPerc (0.1,lineThicknessThin) $
    withColor "salmon" $
    stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash..."
  waitOn $ mapM (fork . flip oShowWith oFadeIn) [ruler,ruleb]

  mapM oHide [sentb,ruleb]
  mapM (fork . flip oShowWith wiggleAnim) [sentb', ruleb']

  waitOn $ mapM (fork . flip oHideWith (reverseA . oDraw)) [sentr, sentb', ruler,ruleb']

completionExample :: Animation
completionExample = scene $ do
  bird <- oNew $ stdLaTeX "bird\\ is\\ a\\ noun"
  pers <- oNew $ stdLaTeX "person\\ is\\ a\\ noun"
  flyi <- oNew $ stdLaTeX "flies\\ is\\ a\\ verb"
  driv <- oNew $ stdLaTeX "drives\\ is\\ a\\ verb"
  sent <- oNew $ stdLaTeX $ "&sentence\\ (the\\ X\\ Y)\\ \\vdash\\\\ &\\qquad X\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  let lst = [bird,pers, flyi,driv,sent]

  sent2 <- oNew $ stdLaTeX $ "sentence\\ (the\\ bird\\ Y)?"
  setCenterPerc (0.5,0.5) sent2
  oShowWith sent2 oDraw
  moveAbsPerc sent2 1 (0,0)

  sent3 <- oNew $ stdLaTeX $ "sentence\\ (the\\ X\\ Y)\\ \\vdash"
  fork $ moveAbsPerc sent2 1 (0,0)
  setTopLeftPerc (0,lineThicknessThin) sent3
  oShowWith sent3 oScaleIn

  sent4 <- oNew $ stdLaTeX $ "sentence\\ (the\\ bird\\ Y)\\ \\vdash"
  setTopLeftPerc (0,lineThicknessThin) sent4
  fork $ oHideWith sent3 oScaleOut
  oShowWith sent4 oScaleIn

  sent5 <- oNew $ stdLaTeX $ "bird\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*2) sent5
  oShowWith sent5 oScaleIn

  --mapM oHide [sentr2,sentx2,ruler2,rulex2, tailr2, tailx2,tailnoun2]
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*3) bird
  setTopLeftPerc (lineThicknessThin,lineThicknessThin*4) pers
  waitOn $ do
    fork $ oShowWith bird oScaleIn
    fork $ oShowWith pers oScaleIn
  wait 1

  let green = "lawngreen"

  (sentrb, sentxb) <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,0) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)?"
  (rulerb, rulexb) <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)\\ \\vdash"
  (tailrb, tailxb) <- oNewTup $ splitGlyphs ([0..3]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"
  (_, sentxb') <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,0) $
    withColor green $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)?"
  (_, rulexb') <- oNewTup $ splitGlyphs [12..15] $
    svgTranslateTopLeftPerc (0,lineThicknessThin) $
    withColor green $
    stdLaTeX $ "sentence\\ (the\\ bird\\ Y)\\ \\vdash"
  (_, tailxb') <- oNewTup $ splitGlyphs ([0..3]) $
    svgTranslateTopLeftPerc (lineThicknessThin,lineThicknessThin*2) $
    withColor green $
    stdLaTeX $ "bird\\ is\\ a\\ noun,\\ Y\\ is\\ a\\ verb"

  mapM oHide [sent2,sent4,sent5]
  mapM oShow [sentrb,sentxb,rulerb,rulexb, tailrb, tailxb]
  wait 1
  mapM oHide [sentxb,rulexb,tailxb]
  mapM oShow [sentxb',rulexb',tailxb']
  oShowWith bird (oColor green)
  mapM oHide [sentrb,sentxb',rulerb,rulexb', tailrb, tailxb']
  mapM oShow [sent2,sent4,sent5]

  oShowWith bird (oColor "white")
  wait 1
  waitOn $ mapM (fork . flip oHideWith oFadeOut) [bird, pers]

  moveAbsPerc sent5 1 (lineThicknessThin/2,lineThicknessThin*2)
  let offset = 4
  setTopLeftPerc (lineThicknessThin*offset,lineThicknessThin*3) flyi
  setTopLeftPerc (lineThicknessThin*offset,lineThicknessThin*4) driv
  waitOn $ do
    fork $ oShowWith flyi oScaleIn
    fork $ oShowWith driv oScaleIn
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

  oShowWith flyi (oColor markingColor)
  mapM oHide [sent2,sent4,sent5]
  mapM oShow [sentr2y,sentx2y,ruler2y,rulex2y, tailr2y, tailx2y,tailnoun2y]

  waitOn $ mapM (fork . flip oShowWith wiggleAnim) [sentx2y,rulex2y,tailx2y,tailnoun2y]

  waitOn $ mapM (fork . flip oHideWith oFadeOut) [flyi,driv]
  waitOn $ mapM (fork . flip oHideWith oFadeOut) [ruler2y,rulex2y, tailr2y, tailx2y, tailnoun2y]

  sent6 <- oNew $ stdLaTeX $ "sentence\\ (the\\ bird\\ flies)."
  setTopLeftPerc (0,0) sent6
  mapM oHide [sentr2y, sentx2y]
  oShow sent6
  moveCenterAbsPerc sent6 1 (0.5,0.5)


intro :: Animation
intro = scene $ do
  let computerScale = 0.4
  numbers <- oNew $ scale computerScale $ center $ loadSVG "./assets/IntroParts/numbers.svg"
  computer <- oNew $ scale computerScale $ center $ loadSVG "./assets/IntroParts/computer.svg"
  computerAnswer <- oNew $ scale computerScale $ center $ loadSVG "./assets/IntroParts/computerAnswer.svg"
  computerTouch <- oNew $ scale computerScale $ center $ loadSVG "./assets/IntroParts/computerTouch.svg"

  let faceScale = 0.3
  face <- oNew $ scale faceScale $ center $ loadSVG "./assets/IntroParts/face.svg"
  finger <- oNew $ scale faceScale $ center $ loadSVG "./assets/IntroParts/finger.svg"

  question <- oNew $ scale computerScale $ center $ loadSVG "./assets/IntroParts/question.svg"
  answer <- oNew $ scale computerScale $ center $ loadSVG "./assets/IntroParts/answer.svg"

  let lst = [computer, computerAnswer, computerTouch, numbers, face, finger, question, answer]
  {-}
  forM (zip lst [(x*0.2,y*0.2) | x <- [1..4], y<- [1..4]]) $ \(o,i) -> do
    setCenterPerc i o
    oShow o
  wait 1
  -}

  oShowWith computer oDraw
  setCenterPerc (0.62,0.5) numbers
  oShowWith numbers (setDuration 1 . oDraw)
  oHideWith numbers (reverseA . setDuration 1 . oDraw)

  let leftX = 0.7
  let rightX = 0.2
  moveCenterAbsPerc computer 1 (leftX,0.5)
  setCenterPerc (rightX,0.5) face
  oShowWith face oFadeIn

  let rightOffset = 0.15
  setCenterPerc (rightX+rightOffset,0.5-0.1) question
  waitOn $ do
    fork $ oShowWith question oFadeIn
    fork $ moveCenterAbsPerc question 1 (rightX+rightOffset,0.5-0.2)

  setCenterPerc (leftX, 0.5-0.22) answer
  waitOn $ do
    fork $ oShowWith answer oFadeIn
    fork $ moveCenterAbsPerc answer 1 (leftX,0.5-0.3)

  oHide question
  oHide answer

  setCenterPerc (leftX,0.5) computerTouch
  setCenterPerc (leftX-0.1,0.5+0.06) finger
  oHide computer
  oShow computerTouch
  oShow finger
  moveCenterAbsPerc finger 1 (leftX-0.1,0.45)
  moveCenterAbsPerc finger 1 (leftX-0.1,0.5+0.06)
  moveCenterAbsPerc finger 1 (leftX-0.15,0.5+0.06)
  moveCenterAbsPerc finger 1 (leftX-0.1,0.5+0.06)
  fork $ oShowWith face $ setDuration 2 . (\svg -> animate $ \t -> rotate (t*20) svg)
  wait 1
  setCenterPerc (leftX,0.5) computerAnswer
  oHide computerTouch
  oShow computerAnswer
  wait 1

--does not work with gradients for some weird reason...
loadSVG :: FilePath -> SVG
loadSVG path = flipYAxis $ last $ _documentElements $ fromJust $ unsafePerformIO $ loadSvgFile path
