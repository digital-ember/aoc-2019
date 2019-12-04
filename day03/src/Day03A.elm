module Day03A exposing(..)

import Maybe exposing(Maybe)
import Dict exposing(Dict)
import Set exposing(..)
import Tuple exposing(..)

type alias WirePath = Dict Coordinate Int

type alias Coordinate = (Int, Int)

type alias CoordinateWithDistance = (Coordinate, Int)

type Dir 
  = Right
  | Left
  | Up
  | Down

{-- original but slow: parts 1 & 2 --}
solve : String -> (Maybe Int, Maybe Int)
solve raw =
  let
    lines = String.lines raw
    wire1 = List.head lines |> Maybe.withDefault ""
    wire2 = List.drop 1 lines |> List.head |> Maybe.withDefault ""
    wirePath1 = parseWire wire1 
    wirePath2 = parseWire wire2
    crossings = Dict.filter (\coord1 d -> Dict.member coord1 wirePath2) wirePath1  
      |> Dict.keys
    closestCrossing 
      = crossings 
      |> List.map (\(x, y) -> (abs x) + (abs y)) 
      |> List.minimum

    minimumDistance 
      = crossings
      |> List.filterMap (calcDistance (wirePath1, wirePath2))
      |> List.minimum 
  in
    (closestCrossing, minimumDistance)
    

calcDistance : (WirePath, WirePath) -> Coordinate -> Maybe Int
calcDistance (path1, path2) crossing =
  let
    mbD1 = Dict.get crossing path1
    mbD2 = Dict.get crossing path2
  in
    case (mbD1, mbD2) of
        (Just d1, Just d2) ->
          Just (d1 + d2)
    
        (_, _) ->
          Nothing
            
            
{-- input parsing --}
parseWire : String -> WirePath
parseWire line =
  let
    instructions = String.split "," line 
    coordinates = List.foldl parseInstruction [] instructions 
  in
    coordinates |> Dict.fromList
    
parseInstruction : String -> List CoordinateWithDistance -> List CoordinateWithDistance
parseInstruction instruction coordinates =
  let
    startingPoint 
      = List.reverse coordinates 
        |> List.head 
        |> Maybe.withDefault ((0, 0), 0)

    instructionSplit = String.uncons instruction
  in
    case instructionSplit of
        Just (dir, length) ->
          List.append coordinates (instructionToCoordinates dir length startingPoint)
    
        Nothing ->
          coordinates --todo: error handling

instructionToCoordinates : Char -> String -> CoordinateWithDistance -> List CoordinateWithDistance
instructionToCoordinates cdir length coordinate =
  let
    steps = String.toInt length |> Maybe.withDefault 0 
  
    mbDir = 
      case cdir of
        'R' -> Just Right    
    
        'L' -> Just Left
                      
        'U' -> Just Up

        'D' -> Just Down            
        
        _ -> Nothing 

  in
    case mbDir of
      Just dir ->
        move coordinate dir steps []
    
      Nothing ->
        [] -- todo: error handling

doMove : CoordinateWithDistance -> Dir -> CoordinateWithDistance 
doMove ((x, y), d) dir =
  case dir of
    Right -> ((x+1, y), d+1)

    Left -> ((x-1, y), d+1)
    
    Up -> ((x, y-1), d+1)
    
    Down -> ((x, y+1), d+1)
  

move : CoordinateWithDistance -> Dir -> Int -> List CoordinateWithDistance -> List CoordinateWithDistance 
move coordinate dir steps coordinates =
  if steps == 0 then
    coordinates
  else 
    let
      coordinateNew = doMove coordinate dir
      coordinatesNew = coordinates ++ [coordinateNew] 
    in
      move coordinateNew dir (steps-1) coordinatesNew
    


