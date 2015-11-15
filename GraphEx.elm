module GraphEx where

import Array 
import Debug 
import GraphPoints exposing (rangeX, rangeY)
import GraphTimeSeries exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Text
import Time exposing ( every )
import Random exposing (..)

points : List (Float, Float)
points =
  [ (1447357860000,0),(1447357920000,136718),(1447357980000,142695),(1447358040000,139104),(1447358100000,141398),(1447358160000,147652),(1447358220000,144264),(1447358280000,139539),(1447358340000,149566),(1447358400000,150078),(1447358460000,167734),(1447358520000,165021),(1447358580000,163600),(1447358640000,162649),(1447358700000,138100),(1447358760000,155245),(1447358820000,160275),(1447358880000,158258),(1447358940000,165195),(1447359000000,168657),(1447359060000,174462),(1447359120000,191707),(1447359180000,163421),(1447359240000,174038),(1447359300000,177712),(1447359360000,172363),(1447359420000,173849),(1447359480000,187460),(1447359540000,155155),(1447359600000,140025),(1447359660000,151682),(1447359720000,0)]

model = 
  {
    rseed = initialSeed 99333, 
    point_list = points,
    element = (collage 1100 300 []) 
  }

main : Signal Element
main =
  Signal.map (\x -> x.element) (Signal.foldp (\x y -> 
     let
        (random, seed) = generate (float 100000 400000) y.rseed
        rX = rangeX y.point_list
        rY = rangeY y.point_list 
        pList = (Array.fromList y.point_list)
      in
        case Array.get ((Array.length pList) - 1) pList of 
          Just (etime, edata) -> 
            { y | element <-  
              (collage 1100 300
                (
                   [ 
--                     (xdash rX),
--                     (ydash rY),
                     (title "My Testing Graph" 20),
                     (drawLine y.point_list rX rY),
                     (drawFill y.point_list rX rY)
                   ] |> xtitle rX 
                     |> ytitle rY
                     |> drawCircle y.point_list rX rY
                     |> background 1100 300 
                 )
               ),
               rseed <- seed,
               point_list <- pList |>  Array.push (etime + 60000, random) |> Array.slice 1 (Array.length pList + 1) |>  Array.toList
             }
          _ -> y
       ) model (every 1000))
