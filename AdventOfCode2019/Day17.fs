module Day17

open Computer

let scaffoldMap = 
    loadProgram "Input/day17.txt"
    |> outputSeq
    |> Seq.map (fun (v,_) -> char v)
    |> Seq.scan( fun ((x,y),last) c -> 
       if last = '\n' 
       then (0, y+1), c
       else (x+1, y), c ) ((0,-1),'\n')
    |> Seq.filter (fun (_,c) -> c <> '\n' && c <> '.' )
    |> Map.ofSeq

let neighbors (x,y) = [ (x-1,y); (x+1,y); (x,y-1); (x,y+1) ]

let isIntersection pos =
    neighbors pos 
    |> List.exists( fun n -> (Map.tryFind n scaffoldMap) <> Some '#' )
    |> not 

let part1 () =
   scaffoldMap
   |> Map.toSeq
   |> Seq.filter (fun (pos,_) -> isIntersection pos )
   |> Seq.toList
   |> Seq.map (fun ((x,y),_) -> x * y)
   |> Seq.sum

let xPosAddr = 576
let yPosAddr = 577
let collectedAddr = 438

let instructionSetsYPos cpu =
    opcode cpu = 1 && yPosAddr = (cpu |> getParamAddr 3)

let setBotPosition (x,y) cpu = 
    cpu 
    |> write xPosAddr (int64 x)
    |> write yPosAddr (int64 y)

let toInputArray input =
    input
    |> Seq.map (sprintf "%s\n")
    |> Seq.concat
    |> Seq.map (fun c -> int64 c)
    |> Seq.toList

let rec runHack unvisited cpu =
    match tick cpu with
    | Some newCpu -> 
        if instructionSetsYPos cpu then
            match unvisited with
            | p::ps -> runHack ps (newCpu |> setBotPosition p)
            | _ -> newCpu |> read collectedAddr
        else runHack unvisited newCpu
    | _ -> failwith "Program halted"

let part2 () =
    let programInput =
        [ "A"; "9999"; "1"; "1"; "n" ] 
        |> toInputArray
    
    let unvisited =
        scaffoldMap 
        |> Map.toList
        |> List.map fst

    loadProgram "Input/day17.txt"
    |> write 0 2L
    |> inputList programInput
    |> runHack unvisited