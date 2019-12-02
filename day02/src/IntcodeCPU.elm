module IntcodeCPU exposing(runProgram)

import Dict exposing(Dict)
import Result exposing(Result(..))
import Maybe exposing(..)


type alias Memory = Dict Int Int

type alias Op = Int -> Int -> Int


runProgram : List Int -> (Int, Int) -> Result String Int 
runProgram input (noun, verb) =
  let 
    mbResult = loadMemory input (noun, verb)
      |> execute 0  
      |> Dict.get 0
  
  in
    Result.fromMaybe "No value in memory at position 0!" mbResult


execute : Int -> Memory -> Memory
execute address memory =
  let
    mbOpcode = Dict.get address memory
  in
    case mbOpcode of
      Just 1 ->
        step address memory (+)

      Just 2 ->
        step address memory (*)

      Just 99 ->
        memory

      _ -> 
        memory


step : Int -> Memory -> Op -> Memory
step address memory op  =
  runOp address memory op
    |> execute (address+4)


runOp : Int -> Memory -> Op -> Memory
runOp key memory op =
  let
      mbLeftKey = Dict.get (key+1) memory
      mbRightKey = Dict.get (key+2) memory
      mbTargetKey = Dict.get (key+3) memory
  in
    case (mbLeftKey, mbRightKey, mbTargetKey) of
        (Just leftKey, Just rightKey, Just targetKey) ->

          let
            mbLeft = Dict.get leftKey memory
            mbRight = Dict.get rightKey memory
          in
            case (mbLeft, mbRight) of
                (Just left, Just right) ->
                  Dict.insert targetKey (op left right) memory 
            
                _ ->
                  memory          
        _ ->
          memory


loadMemory : List Int -> (Int, Int) -> Memory
loadMemory rawData (noun, verb) =
  rawData 
    |> List.indexedMap (\index element -> (index, element))
    |> Dict.fromList
    |> Dict.insert 1 noun 
    |> Dict.insert 2 verb
