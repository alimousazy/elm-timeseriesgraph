module GraphPoints (rangeX, rangeY, Range, Point) where

type alias Range =
  { min : Float, max : Float }

type alias Point =
  { x : Float, y : Float }

pointRange: Range
pointRange =
  { min = -1, max = 0 }

updateMin: Float -> Range -> Range
updateMin x info =
  if x < info.min || info.min == -1 then 
     { info | min <- x }
  else
     info

updateMax: Float -> Range -> Range
updateMax x info =
  if x > info.max then 
     { info | max <- x }
  else
     info

rangeX : List (Float, Float) -> Range
rangeX list =
        List.foldl (\item init -> 
          case item of
            (x, y) -> (updateMax x (updateMin x init))
         ) pointRange list 

rangeY : List (Float, Float) -> Range
rangeY list =
        List.foldl (\item init -> 
          case item of
            (x, y) -> (updateMax y (updateMin y init))
         ) pointRange list 


