module Day13

open System
open Computer

type Point = { X:int; Y:int }
type Game = { CPU : Computer option; Score : int; Screen : Map<(int*int),int> }

let newGame = 
    { CPU = Some <| loadProgram "Input/day13.txt"
      Score = 0
      Screen = Map.empty }

let findTileX n g =
    g.Screen
    |> Map.toSeq
    |> Seq.tryFind ( fun (_,v) -> v = n )
    |> Option.map ( fun ((x,_),_) -> x )

let setInput v g =
    { g with CPU = g.CPU |> Option.map( fun cpu -> { cpu with Input = [ int64 v ] } ) }

let updateInput g = 
    match findTileX 3 g, findTileX 4 g with
    | Some bx, Some px -> g |> setInput (Math.Clamp( px - bx, -1, 1 ))
    | _,_ ->  g |> setInput 0

let rec getNextOutput n cpu =
    if n > 0 then
        match cpu |> Option.map nextOutput |> Option.flatten with
        | Some (next,nextCpu) ->
            let rest,restCpu = getNextOutput (n-1) (Some nextCpu)
            (int next)::rest, restCpu
        | None -> [], None
    else [],cpu 

let procOutput draw g =
    match g.CPU |> getNextOutput 3 with 
    | [-1; 0; score ], cpu -> 
        draw 1 1 score
        { g with Score = score; CPU = cpu }
    | [ x; y; t ], cpu -> 
        draw x y t
        { g with Screen = g.Screen |> Map.add (x,y) t; CPU = cpu }
    | _, cpu -> { g with CPU = cpu }
    
let rec run draw g =
    if g.CPU.IsSome then 
        procOutput draw g 
        |> updateInput
        |> run draw
    else g

let tiles = [ ' '; '█'; '░'; '╤'; '○' ] |> List.map (fun c -> String [| c |])

let noRender x y t = ()
let drawConsole xOffset yOffset x y t =
    let origX,origY = Console.CursorLeft, Console.CursorTop
    Console.SetCursorPosition( min (Console.BufferWidth - 1) (xOffset + x), yOffset + y )
    Console.Write( tiles |> List.tryItem t |> Option.defaultValue (t.ToString()) )
    Console.SetCursorPosition( origX, origY )

let part1 () =
    newGame 
    |> run noRender
    |> fun g -> g.Screen
    |> Map.toSeq 
    |> Seq.filter( fun (_,v) -> v = 2 )
    |> Seq.length

let freeplay g = { g with CPU = g.CPU |> Option.map( write 0 2L ) }

let part2 () =
    newGame 
    |> freeplay 
    |> run (drawConsole 50 (Console.WindowTop+1))
    |> fun g -> g.Score