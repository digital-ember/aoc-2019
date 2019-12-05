module Day04 exposing(..)

part1 (lowest, highest) =
    part1Passwords (lowest, highest)
      |> List.length 
  
part1Passwords (lowest, highest) = 
  List.range lowest highest
      |> List.filter passwordRules

passwordRules num =
  rule num (==) (||)
  && rule num (<=) (&&)

rule num comparer binaryLogic =
  let
    d6 = num // 100000
    d5 = (Basics.modBy 100000 num) // 10000 
    d4 = (Basics.modBy 10000 num) // 1000 
    d3 = (Basics.modBy 1000 num) // 100 
    d2 = (Basics.modBy 100 num) // 10 
    d1 = Basics.modBy 10 num
  in
    binaryLogic (comparer d6 d5) (comparer d5 d4)
    |> binaryLogic (comparer d4 d3)
    |> binaryLogic (comparer d3 d2) 
    |> binaryLogic (comparer d2 d1)

part2 (lowest, highest) =
  let
    p1 = part1Passwords (lowest, highest)
    p2 = List.filter rulePart2 p1 |> Debug.log "p2: "
  in
    List.length p2

rulePart2 num  =
  let
    d6 = num // 100000
  in
    rulePart2Rec num d6 100000 1

rulePart2Rec num dLast log count =
  if log == 1 then
    count == 2
  else 
    let
      logNew = log // 10
      dNew = (Basics.modBy log num) // logNew 
      isMatch = dNew == dLast
      countNew = if isMatch then count+1 else count
    in
      if isMatch then
        if countNew > 2 then 
          rulePart2Rec num dNew logNew countNew
        else --exactly two matches, look for third
          rulePart2Rec num dNew logNew countNew
      else
        if countNew == 2 then 
          True -- we found an exact match of two, not more not less
        else
          rulePart2Rec num dNew logNew 1
     
  
    

input = (367479, 893698)