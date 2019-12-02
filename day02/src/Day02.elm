module Day02 exposing(..)

import Maybe exposing(Maybe)
import Dict exposing(Dict)
import Array exposing (Array)
import IntcodeCPU

{-- part 1 --}
part1 : Int
part1 =
  let 
    result = IntcodeCPU.runProgram input (12, 2)
  in
    case result of 
      Ok value ->
        value

      Err msg ->
        Debug.log msg -1
      

{-- part 2 --}
part2 : Int
part2 =
  let
    result = findGravityAssistInput 0 allOpPairs input gravityAssistValue
  in
    case result of
      Ok (noun, verb) ->
        100 * noun + verb  
  
      Err msg ->
        Debug.log msg -1
            
    

findGravityAssistInput : Int -> Array (Int, Int) -> List Int -> Int -> Result String (Int, Int)
findGravityAssistInput candidateIndex allCandidates rawInput valueToFind =
  let
    mbCandidate = Array.get candidateIndex allCandidates
  in
    case mbCandidate of
      Just (noun, verb) -> 
        let            
          resultValue = IntcodeCPU.runProgram rawInput (noun, verb)
        in
          case resultValue of
              Ok value ->
                if value == valueToFind then
                  Ok (noun, verb)
                else
                  findGravityAssistInput (candidateIndex+1) allCandidates rawInput valueToFind

              Err msg ->
                Err  ("Couldn't calculate value for (" ++ (String.fromInt noun) ++ ", " ++ (String.fromInt verb) ++ ") due to error: " ++ msg)  

      Nothing ->
        Err ("no more candidates: value " ++ (String.fromInt valueToFind) ++ " could not be found!")


gravityAssistValue : Int       
gravityAssistValue =
  19690720

allOpPairs : Array (Int, Int)
allOpPairs =
  List.range 0 99
    |> List.repeat 100
    |> List.indexedMap initOpTuple
    |> List.concat
    |> Array.fromList


initOpTuple : Int -> List Int -> List (Int, Int)
initOpTuple i list =
  List.map (Tuple.pair i) list


input = 
  [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,5,23,2,23,9,27,1,5,27,31,1,9,31,35,1,35,10,39,2,13,39,43,1,43,9,47,1,47,9,51,1,6,51,55,1,13,55,59,1,59,13,63,1,13,63,67,1,6,67,71,1,71,13,75,2,10,75,79,1,13,79,83,1,83,10,87,2,9,87,91,1,6,91,95,1,9,95,99,2,99,10,103,1,103,5,107,2,6,107,111,1,111,6,115,1,9,115,119,1,9,119,123,2,10,123,127,1,127,5,131,2,6,131,135,1,135,5,139,1,9,139,143,2,143,13,147,1,9,147,151,1,151,2,155,1,9,155,0,99,2,0,14,0]

test1 =
  [1,0,0,0,99]


test2 =
  [2,3,0,3,99]

test3 =
  [2,4,4,5,99,0]

test4 =
  [1,1,1,4,99,5,6,0,99]