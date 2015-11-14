module GraphTimeSeries (xdash, ydash, releativePosY, releativePosX, drawPoints, xtitle, ytitle, title)  where

import GraphPoints exposing (rangeX, rangeY, Range, Point)
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
  { defaultStyle |  height <-  Just 20 }

tickStyle: Text.Style
tickStyle = 
  { defaultStyle |  height <-  Just 10 }


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
    style = { defaultStyle |  height <-  Just size }
  in
    (text (Text.style  style  (Text.fromString "Testing Graph"))) |> move (releativePosX (drawSpec.width / 2) ,  releativePosY drawSpec.height + 20)


drawPoints: List (Float, Float) -> Range -> Range -> Form
drawPoints points rX rY = 
  let 
      p =  (rX.min, rY.min) :: points
      pos = (List.map (\x -> 
                case x of 
                  (xA, yA) -> 
                    (
                      newValue rX { min = releativePosX 0, max = releativePosX drawSpec.width } xA, 
                      newValue rY { min = releativePosY 0, max = releativePosY drawSpec.height } yA
                    )
        ) p)
  in 
    filled (rgba 22 22 99 0.2) (polygon (List.append pos [ (releativePosX drawSpec.width, releativePosY 0) ]))
--    outlined (solid (rgba 255 22 99 0.2)) (polygon (List.append pos [ (releativePosX drawSpec.width, releativePosY 0) ]))

ydash: Range -> Form
ydash range = 
  let
    inc = abs (newValue range { min = 0, max = drawSpec.height } 50000)
    end =  (releativePosY drawSpec.height) + inc
  in
    traced (solid red) (path (ydashpoints { x = releativePosX 0, y = releativePosY 0 } end inc))

ydashpoints: Point -> Float -> Float ->  List (Float, Float)
ydashpoints point end inc = 
  if end < point.y then
     []
  else
    (point.x, point.y) ::
    (point.x - 10,  point.y) ::
    (point.x,  point.y) ::
    (ydashpoints { y = point.y + inc, x =  point.x } end inc)


xdash: Range -> Form
xdash range = 
  traced (solid red) (path (xdashpoints { x = releativePosX 0, y = releativePosY 0} (releativePosX drawSpec.width) drawSpec.inc))


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
    end =  (releativePosY drawSpec.height) + inc
  in
    ytitlespoints { x = releativePosX 0, y = releativePosY 0} end inc range list

ytitlespoints: Point -> Float -> Float -> Range ->  List Form -> List Form 
ytitlespoints point end inc range list = 
  if end < point.y then
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
    p =  (toString (Date.hour x)) ++ ":" ++ (toString (Date.minute x)) 
  in 
    if Date.minute x < 10 then
      p ++ "0"
    else
      p


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

points : List (Float, Float)
points =
  [ (1447357860000,0),(1447357920000,136718),(1447357980000,142695),(1447358040000,139104),(1447358100000,141398),(1447358160000,147652),(1447358220000,144264),(1447358280000,139539),(1447358340000,149566),(1447358400000,150078),(1447358460000,167734),(1447358520000,165021),(1447358580000,163600),(1447358640000,162649),(1447358700000,138100),(1447358760000,155245),(1447358820000,160275),(1447358880000,158258),(1447358940000,165195),(1447359000000,168657),(1447359060000,174462),(1447359120000,191707),(1447359180000,163421),(1447359240000,174038),(1447359300000,177712),(1447359360000,172363),(1447359420000,173849),(1447359480000,187460),(1447359540000,155155),(1447359600000,140025),(1447359660000,151682),(1447359720000,0)]

main : Element
main =
  let 
      rX = rangeX points
      rY = rangeY points
  in 
    collage 1200 900
    (
       [ 
         (xdash rX),
         (ydash rY),
         (text (Text.style  textStyle  (Text.fromString "Testing Graph"))) |> move (releativePosX (drawSpec.width / 2) ,  releativePosY drawSpec.height + 20),
         (drawPoints points rX rY)
       ] |> xtitle rX 
         |> ytitle rY
     )
