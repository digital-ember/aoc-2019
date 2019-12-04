module Day03B exposing(..)

import Input exposing(..)

type alias Coordinate = { x : Int, y : Int}

type Orientation
  = H 
  | V
  | Zero

type alias Segment = 
  { start : Coordinate
  , end : Coordinate
  , orientation : Orientation
  , wireLength : Int
  , segmentLength : Int
  }

{-- faster solution: parts 1 & 2 --}
solveNew =
  let
    lines = String.lines input
    wire1 = List.head lines |> Maybe.withDefault ""
    wire2 = List.drop 1 lines |> List.head |> Maybe.withDefault ""

    segmentsWire1 = segments wire1 
    segmentsWire2 = segments wire2 

    results = findIntersectionDistances segmentsWire1 segmentsWire2 []
    
    part1 = List.map (\t -> Tuple.first t) results
      |> List.filter (\i -> i /= 0)
      |> List.minimum

    part2 = List.map (\t -> Tuple.second t) results
      |> List.filter (\i -> i /= 0)
      |> List.minimum

  in
    (part1, part2)

zeroSegment =
  {start = zeroCoordinate, end = zeroCoordinate, orientation = Zero, wireLength = 0, segmentLength = 0}

segments : String -> List Segment
segments wire =
  let
    instructions = String.split "," wire 
  in
    segment instructions zeroSegment []


segment : List String -> Segment -> List Segment -> List Segment
segment instructions segmentLast segmentList =
  case instructions of
    [] ->
      segmentList   

    nextInstruction :: tail ->
      let
        instructionSplit = String.uncons nextInstruction
      in
        case instructionSplit of
          Nothing ->
            segmentList

          Just (dir, length) ->
            let 
              steps = String.toInt length |> Maybe.withDefault 0
              newEnd = calcNewEnd dir steps segmentLast.end
              segmentTemp = 
                { segmentLast 
                  | start = segmentLast.end
                  , end = newEnd
                  , wireLength = segmentLast.wireLength + steps
                  , segmentLength = steps 
                }

              segmentNext =  
                case segmentLast.orientation of
                  H ->
                    { segmentTemp | orientation = V } 
              
                  V ->
                    { segmentTemp | orientation = H }
                
                  Zero ->
                    if dir == 'U' || dir == 'D' then
                      { segmentTemp | orientation = V }
                    else if dir == 'L' || dir == 'R' then
                      { segmentTemp | orientation = H } 
                    else 
                      segmentLast
            in
              segment tail segmentNext (segmentList ++ [segmentNext])

zeroCoordinate =
  { x = 0, y = 0 }

calcNewEnd dir length end =
  case dir of
    'R' -> { end | x = end.x+length }   

    'L' -> { end | x = end.x-length }
                  
    'U' -> { end | y = end.y-length }

    'D' -> { end | y = end.y+length }            
    
    _ -> end 


findIntersectionDistances : List Segment -> List Segment -> List (Int, Int) -> List (Int, Int)
findIntersectionDistances segments1 segments2 distances =
  case segments2 of
    [] ->
      distances
  
    nextSegment :: tail ->
      let
        segmentsToCheck = 
          case nextSegment.orientation of
            H ->
              List.filter (\s -> s.orientation == V) segments1
        
            V ->
              List.filter (\s -> s.orientation == H) segments1

            Zero ->
              []

        distancesNew = 
              distances ++ (findIntersections nextSegment segmentsToCheck [])
      in
        findIntersectionDistances segments1 tail distancesNew
              
findIntersections segmentWire2 segmentsWire1 distances =
  case segmentsWire1 of
    [] ->
      distances
        
    segmentWire1 :: tail ->
      let
        (segmentV, segmentH) =
          case (segmentWire1.orientation, segmentWire2.orientation) of 
            (V, H) -> (segmentWire1, segmentWire2)

            (H, V) -> (segmentWire2, segmentWire1)

            (_, _) -> (zeroSegment, zeroSegment)
      in
        if segmentV == zeroSegment && segmentH == zeroSegment then
          findIntersections segmentWire2 tail distances
        else
          let
            vStart = min segmentV.start.y segmentV.end.y
            vEnd = max segmentV.start.y segmentV.end.y
            vConst = segmentV.start.x 
            hStart = min segmentH.start.x segmentH.end.x
            hEnd = max segmentH.start.x segmentH.end.x
            hConst = segmentH.start.y 
          in
            if hStart <= vConst && hEnd >= vConst && vStart <= hConst && vEnd >= hConst then
              let
                distanceForCrossing = calcWireDistanceForCrossing segmentH segmentV hConst vConst
                manhattenDistanceX = ((abs hConst) + (abs vConst))
                distancesNew = distances ++ [(manhattenDistanceX, distanceForCrossing)]
              in
                findIntersections segmentWire2 tail distancesNew
              
            else
              findIntersections segmentWire2 tail distances
      
calcWireDistanceForCrossing segmentH segmentV horiz vert =
  let
    dist1 = segmentH.wireLength - segmentH.segmentLength + (abs (segmentH.start.x - vert))
    dist2 = segmentV.wireLength - segmentV.segmentLength + (abs (segmentV.start.y - horiz))
  in
    dist1 + dist2