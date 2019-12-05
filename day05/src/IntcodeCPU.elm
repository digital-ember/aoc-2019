module IntcodeCPU exposing(..)

import Dict exposing(Dict)
import Result exposing(Result(..))
import Maybe exposing(..)


type alias Memory = Dict Int Int

type Op  
  = Add 
  | Mul 
  | Read 
  | Write 
  | JumpIfTrue
  | JumpIfFalse
  | LowerThan
  | Equal
  | Halt
  | Unknown

type alias BinaryOp =
  Int -> Int -> Int

type Mode 
  = Position
  | Immediate

type alias Instruction =
  { op : Op
  , modes : List Mode
  }

type alias OpKeys = 
  { left : Int
  , right : Int
  , target : Int
  }

runProgram : List Int -> Result String Memory
runProgram input =
  let 
    mbResult = loadMemory input
      |> execute 0 

    r = mbResult |> Dict.values |> Debug.log "memory"
  
  in
    Result.fromMaybe "No value in memory at position 0!" (Just mbResult)


parseInstruction : Int -> Instruction 
parseInstruction instructionCode =
  let
    mbOp = parseOpCode instructionCode
    mbModes = Maybe.map (parseModes instructionCode) mbOp
  in
    case (mbOp, mbModes) of
      (Just op, Just modes) ->
        Instruction op modes       
  
      (_, _) -> 
        Instruction Unknown []
            
  
parseOpCode : Int -> Maybe Op
parseOpCode instructionCode =
  let
    instructionDigit = (modBy 100 instructionCode)
  in
    case instructionDigit of
      1 -> Just Add
  
      2 -> Just Mul

      3 -> Just Read

      4 -> Just Write

      5 -> Just JumpIfTrue

      6 -> Just JumpIfFalse

      7 -> Just LowerThan

      8 -> Just Equal

      99 -> Just Halt

      _ -> Nothing

            
parseModes : Int -> Op -> List Mode
parseModes instructionCode op =
  let 
    exp =
      case op of
        Add -> 5

        Mul -> 5

        Read -> 3 
        
        Write -> 3

        JumpIfTrue -> 5
        
        JumpIfFalse -> 5

        LowerThan -> 5

        Equal -> 5

        Halt -> 0

        Unknown -> 0
  in 
      parseMode instructionCode exp []        


parseMode : Int -> Int -> List Mode -> List Mode
parseMode instructionCode exp modes =
  if exp < 3 || exp > 5 then
    modes |> List.reverse
  else
    let
      modeCode = (modBy (10^exp) instructionCode) // (10^(exp-1)) 
      mbMode = modeCodeToMode modeCode 
              
    in
      case mbMode of
        Just mode ->
          parseMode instructionCode (exp-1) (modes++[mode]) 
    
        Nothing ->
          []
              
      
modeCodeToMode : Int -> Maybe Mode
modeCodeToMode modeCode =
  case modeCode of 
    0 -> Just Position

    1 -> Just Immediate

    _ -> Nothing
      

execute : Int -> Memory -> Memory
execute address memory =
  let
    mbInstruction = Dict.get address memory |> Maybe.map parseInstruction --|> Debug.log "instruction"
  in
    case mbInstruction of
      Just instruction ->
        if instruction.op == Unknown || instruction.op == Halt then
          memory
        else
          step address memory instruction

      Nothing ->
        memory


step : Int -> Memory -> Instruction -> Memory
step key memory instruction  =
  let
    (keyNew, memoryNew) = runOp key memory instruction
  in 
    execute (keyNew + (pointerOffsetFromOp instruction.op)) memoryNew


pointerOffsetFromOp : Op -> Int
pointerOffsetFromOp op =
  case op of
      Add -> 4
  
      Mul -> 4

      JumpIfTrue -> 3

      JumpIfFalse -> 3

      LowerThan -> 4

      Equal -> 4

      Read -> 2

      Write -> 2

      Halt -> 0

      Unknown -> 0
          

runOp : Int -> Memory -> Instruction -> (Int, Memory)
runOp key memory instruction =
  let
      (k, v) = (key, Dict.get key memory)
      opdd = instruction
  in
  
  case instruction.op of
      Add -> (key, binaryOp key memory instruction.modes (+))
  
      Mul -> (key, binaryOp key memory instruction.modes (*))

      Read -> (key, read key memory)

      Write -> (key, write key memory instruction.modes)

      JumpIfTrue -> (jump key memory instruction.modes ((/=) 0), memory)

      JumpIfFalse -> (jump key memory instruction.modes ((==) 0), memory)

      LowerThan -> (key, binaryOp key memory instruction.modes lowerThan)

      Equal -> (key, binaryOp key memory instruction.modes equal)

      Halt -> (key, memory)

      Unknown -> (key, memory)

lowerThan : Int -> Int -> Int
lowerThan a b = 
  if a < b then 1 else 0

equal : Int -> Int -> Int
equal a b = 
  if a == b then 1 else 0
  

jump : Int -> Memory -> List Mode -> (Int -> Bool) -> Int
jump key memory modes zeroComparison =
  let
    (_, opKeys) = List.foldl (opKeysBinary key memory) (0, OpKeys 0 0 0) modes
  in
    let
      mbLeft = Dict.get opKeys.left memory
      mbRight = Dict.get opKeys.right memory
    in
      case (mbLeft, mbRight) of
        (Just left, Just right) ->
          if zeroComparison left then
            right - (pointerOffsetFromOp JumpIfFalse)
          else
            key
        _ ->
          key  


read : Int -> Memory -> Memory
read key memory =
  let
    mbTarget = Dict.get (key+1) memory
  in
    case mbTarget of
      Just target ->
        Dict.insert target 5 memory

      Nothing ->
        memory

write : Int -> Memory -> List Mode -> Memory
write key memory modes = 
  let
    mbMode = List.head modes 
  in
    case mbMode of
        Just mode ->
          case mode of
              Immediate ->
                doWrite (key+1) memory
          
              Position ->
                let
                  mbPosition = Dict.get (key+1) memory 
                in
                
                  case mbPosition of
                    Just position ->
                      doWrite position memory                             
                    Nothing ->
                      memory 
    
        Nothing ->
          memory |> Debug.log "could not write!"
    
doWrite position memory =
  let
    mbTarget = Dict.get position memory
  in
    case mbTarget of
        Just target ->
          let
            err = target |> Debug.log "ERROR: "
          in
            memory
    
        Nothing ->
          memory     

binaryOp : Int -> Memory -> List Mode -> BinaryOp -> Memory
binaryOp key memory modes binOp =
  let
    (_, opKeys) = List.foldl (opKeysBinary key memory) (0, OpKeys 0 0 0) modes --|> Debug.log "opKeys"
  
  in
    let
      mbLeft = Dict.get opKeys.left memory
      mbRight = Dict.get opKeys.right memory
    in
      case (mbLeft, mbRight) of
        (Just left, Just right) ->
          Dict.insert opKeys.target (binOp left right) memory 
    
        _ ->
          memory  
  

opKeysBinary : Int -> Memory -> Mode -> (Int, OpKeys) -> (Int, OpKeys)
opKeysBinary key memory mode (index, opKeys) =
  case mode of
    Position ->
      let
        mbLeftKey = Dict.get (key+1) memory
        mbRightKey = Dict.get (key+2) memory
        mbTargetKey = Dict.get (key+3) memory
      in
        case (mbLeftKey, mbRightKey, mbTargetKey) of
          (Just leftKey, Just rightKey, Just targetKey) ->
            case index of
              0 ->
                (1, { opKeys | left = leftKey })
                
              1 ->
                (2, { opKeys | right = rightKey })

              2 -> 
                (3, { opKeys | target = targetKey })

              _ -> 
                (index, opKeys)

          _ ->
            (index, opKeys)
    
    Immediate ->
      case index of
        0 ->
          (1, { opKeys | left = key+1 })
          
        1 ->
          (2, { opKeys | right = key+2 })

        2 -> 
          (3, { opKeys | target = key+3 })

        _ -> 
          (index, opKeys)
  

loadMemory : List Int -> Memory
loadMemory rawData =
  rawData 
    |> List.indexedMap (\index element -> (index, element))
    |> Dict.fromList

