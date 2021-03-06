module GraphTimeSeries (xdash, ydash, releativePosY, releativePosX, drawLine, xtitle, ytitle, title, drawCircle, background, drawFill, yLine, xLine)  where

import GraphPoints exposing (rangeX, rangeY, Range, Point, Points)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Random exposing  (int, generate, initialSeed, Seed)
import List exposing (..)
import Array 
import String
import Time exposing (..)
import Debug 
import Date
import Text exposing (defaultStyle)

textStyle: Text.Style
textStyle = 
  { defaultStyle |  
        height <-  Just 20,
        color  <-  red
  }

tickStyle: Text.Style
tickStyle = 
  { defaultStyle |  height <-  Just 10, color <- (rgb 200 200 200) }


drawSpec: { width : Float, height : Float, inc : Float }
drawSpec = 
  { width = 1000.0, height = 200.0, inc = 40.0} 

newValue: Range -> { min : Float, max : Float } -> Float -> Float
newValue range spec oldValue = 
  let 
      oldMin = range.min
      oldMax = range.max
      newMin = spec.min
      newMax = spec.max
  in 
      (((oldValue - oldMin) * (newMax - newMin)) / (oldMax - oldMin)) + newMin

oldValue: Range -> { min : Float, max : Float } -> Float -> Float
oldValue range spec newValue = 
  let 
      oldMin = range.min
      oldMax = range.max
      newMin = spec.min
      newMax = spec.max
  in 
     (((newValue - newMin) * (oldMax - oldMin)) / (newMax - newMin)) + oldMin 


releativePosX: Float -> Float 
releativePosX x = 
  x - (drawSpec.width / 2.0)

releativePosY: Float -> Float
releativePosY y = 
  y - (drawSpec.height / 2.0)

title: String -> Float -> Form 
title str size = 
  let 
    style = { defaultStyle |  height <-  Just size, color <- (rgb 200 200 200) }
  in
    (text (Text.style  style  (Text.fromString "Testing Graph"))) |> move (releativePosX (drawSpec.width / 2) ,  releativePosY 270)

drawCircle: Points -> Range -> Range -> List Form -> List Form
drawCircle points rX rY init = 
  (List.foldl (\x i -> 
            case x of 
              (xA, yA) -> 
                let 
                  xPos = (newValue rX { min = releativePosX 0, max = releativePosX drawSpec.width } xA)
                  yPos = (newValue rY { min = releativePosY 0, max = releativePosY drawSpec.height } yA)
                  p = circle 4 |> filled points.color |>  move (xPos, yPos)
                in
                  p :: i
   ) init  points.points)
  

drawFill: Points -> Range -> Range -> Form
drawFill points rX rY  = 
  let 
    p =  (rX.min, rY.min) :: points.points
    pos = (List.map (\x -> 
                case x of 
                  (xA, yA) -> 
                    (
                      newValue rX { min = releativePosX 0, max = releativePosX drawSpec.width } xA, 
                      newValue rY { min = releativePosY 0, max = releativePosY drawSpec.height } yA
                    )
        ) p)
  in 
    filled points.fillColor (polygon (List.append pos [ (releativePosX drawSpec.width, releativePosY 0) ]))



drawLine: Points -> Range -> Range -> Form
drawLine points rX rY = 
  let 
    pos = (List.map (\x -> 
                case x of 
                  (xA, yA) -> 
                    (
                      newValue rX { min = releativePosX 0, max = releativePosX drawSpec.width } xA, 
                      newValue rY { min = releativePosY 0, max = releativePosY drawSpec.height } yA
                    )
        ) points.points)
    outLineStyle = solid points.color
  in 
    traced { outLineStyle | width <- 2 } pos

yLine : Range -> List Form -> List Form
yLine range list = 
  let
    inc = abs (newValue range { min = 0, max = drawSpec.height } 50000)
    end = (releativePosY (newValue range { min = 0, max = drawSpec.height } range.max)) 
  in
    yLinepoints { x = releativePosX 0, y = releativePosY 0 } end inc list 

yLinepoints: Point -> Float -> Float -> List Form -> List Form
yLinepoints point end inc list = 
  if end + inc < point.y then
    list 
  else
    traced (solid (rgba 200 200 200 0.3)) (path [(point.x, point.y), (releativePosX drawSpec.width, point.y)]) :: 
        (yLinepoints { y = point.y + inc, x =  point.x } end inc list)

xLine: Range -> List Form -> List Form
xLine range list = 
  let 
    yinc = abs (newValue range { min = 0, max = drawSpec.height } 50000)
    yend = newValue range { min = 0, max = drawSpec.height } range.max 
    height = releativePosY (yinc * (toFloat (ceiling (yend / yinc))))
  in
    xLinepoints { x = releativePosX 0, y = releativePosY 0} (releativePosX drawSpec.width) drawSpec.inc height list

xLinepoints: Point -> Float -> Float -> Float -> List Form -> List Form 
xLinepoints point end inc height list = 
  if end < point.x then
    list 
  else
    traced (solid (rgba 200 200 200 0.3)) (path [(point.x, point.y), (point.x, height)]) :: xLinepoints { x = point.x + inc, y =  point.y } end inc height list


ydash: Range -> Color -> Form
ydash range color = 
  let
    inc = abs (newValue range { min = 0, max = drawSpec.height } 50000)
    end =  (releativePosY drawSpec.height) + inc
  in
    traced (solid color) (path (ydashpoints { x = releativePosX 0, y = releativePosY 0 } end inc))

ydashpoints: Point -> Float -> Float ->  List (Float, Float)
ydashpoints point end inc = 
  if end < point.y then
     []
  else
    (point.x, point.y) ::
    (point.x - 10,  point.y) ::
    (point.x,  point.y) ::
    (ydashpoints { y = point.y + inc, x =  point.x } end inc)


xdash: Range -> Color -> Form
xdash range color = 
  traced (solid color) (path (xdashpoints { x = releativePosX 0, y = releativePosY 0} (releativePosX drawSpec.width) drawSpec.inc))

xdashpoints: Point -> Float -> Float ->  List (Float, Float)
xdashpoints point end inc = 
  if end < point.x then
     []
  else
    (point.x, point.y) ::
    (point.x, point.y - 10) ::
    (point.x, point.y) ::
    (xdashpoints { x = point.x + inc, y =  point.y } end inc)

ytitle: Range -> List Form -> List Form 
ytitle range list = 
  let
    inc = abs (newValue range { min = 0, max = drawSpec.height } 50000)
    end = releativePosY (newValue range { min = 0, max = drawSpec.height } range.max) 
  in
    ytitlespoints { x = releativePosX 0, y = releativePosY 0} end inc range list

ytitlespoints: Point -> Float -> Float -> Range ->  List Form -> List Form 
ytitlespoints point end inc range list = 
  if end + inc < point.y then
    list 
  else
    let 
      m = toString  (round (oldValue range { min = releativePosY 0, max = releativePosY drawSpec.height } point.y)) |> yformat
      dText = (text (Text.style  tickStyle  (Text.fromString m))) |> move (point.x - 30, point.y)
    in
      dText :: ytitlespoints { x = point.x , y =  point.y + inc } end inc range list

yformat: String -> String
yformat str = 
   if str == "" then
    ""
   else
    if String.length str <= 3 then
       str
    else
       (String.left 3 str) ++ "." ++ (yformat (String.dropLeft 3 str))

xformat: Date.Date -> String
xformat x = 
  let 
    p =  (toString (Date.hour x)) ++ ":" 
  in 
    if Date.minute x < 10 then
      p ++ "0" ++  (toString (Date.minute x)) 
    else
      p ++ (toString (Date.minute x)) 

background: Float -> Float -> List Form -> List Form 
background height width list = 
 (filled black (rect height width)) :: list

xtitle: Range -> List Form -> List Form 
xtitle range list = 
  xtitlespoints { x = releativePosX 0, y = releativePosY 0} (releativePosX drawSpec.width) drawSpec.inc range list

xtitlespoints: Point -> Float -> Float -> Range ->  List Form -> List Form 
xtitlespoints point end inc range list = 
  if end < point.x then
    list 
  else
    let 
      time = oldValue range { min = releativePosX 0, max = releativePosX drawSpec.width } point.x 
      x = Date.fromTime time
      m =  x |> xformat
      dText = (text (Text.style  tickStyle  (Text.fromString m))) |> move (point.x, point.y - 20)
    in
      dText :: xtitlespoints { x = point.x + inc, y =  point.y } end inc range list

