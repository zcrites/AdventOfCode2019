module Day15

open Computer

let inputMove dir =
    input (int64 dir) >> nextOutput >> Option.defaultWith (fun _ -> failwith "Program halted")

type Tile = 
    { Pos : (int*int); Type : int64; Dist : int; Bot : Computer option  }

type Space = 
    { Oxygen : Tile option; MaxDist : int; Tiles : Map<(int*int),Tile> }

let updateMaxDist t s =
    if t.Type = 1L then { s with MaxDist = max s.MaxDist t.Dist } else s

let updateOxygen t s =
    if t.Type = 2L then { s with Oxygen = Some t } else s

let updateTiles t s =
    { s with Tiles = s.Tiles |> Map.add t.Pos t }

let unexploredNeighbors tile cpu visited =
    [1..4] |> Seq.map( fun dir ->
        let x,y = tile.Pos
        inputMove dir cpu,
        match dir with
        | 1 -> (x, y + 1)
        | 2 -> (x, y - 1)
        | 3 -> (x - 1, y)
        | 4 | _ -> (x + 1, y) )
    |> Seq.filter (fun (_,pos) -> visited |> Map.containsKey pos |> not )
    |> Seq.map (fun ((tileType,cpu),pos) -> 
        { Pos = pos
          Type = tileType
          Bot = if tileType <> 0L then Some cpu else None
          Dist = tile.Dist + 1 } )

let rec explore space tile = 
    let space = 
        space |> updateTiles tile |> updateMaxDist tile |> updateOxygen tile

    tile.Bot 
    |> Option.map ( fun cpu -> unexploredNeighbors tile cpu space.Tiles )
    |> Option.defaultValue Seq.empty
    |> Seq.fold explore space
    
let start = { Pos = (0,0); Type = 1L; Dist = 0; Bot = Some <| loadProgram "Input/day15.txt" }
let emptySpace = { Oxygen = None; MaxDist = 0; Tiles = Map.empty }

let exploredSpace = 
    start |> explore emptySpace

let part1 () =
    exploredSpace.Oxygen 
    |> Option.map( fun oxygen -> oxygen.Dist ) 
    |> Option.defaultWith (fun _ -> failwith "Oxygen not found")

let part2 () =
    exploredSpace.Oxygen 
    |> Option.map ( fun oxygen -> explore emptySpace { oxygen with Dist = 0 } ) 
    |> Option.map ( fun filled -> filled.MaxDist )
    |> Option.defaultWith (fun _ -> failwith "Oxygen not found")