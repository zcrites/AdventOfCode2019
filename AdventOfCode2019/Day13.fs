module Day13

open System
open Computer

type Point = { X:int; Y:int }
type Game = { CPU : Computer option; Score : int; Screen : Map<(int*int),int>; BallX : int option; PaddleX : int option }

let newGame = 
    { CPU = Some <| loadProgram "Input/day13.txt"
      Score = 0
      Screen = Map.empty 
      BallX = None
      PaddleX = None }

let setInput v g =
    { g with CPU = g.CPU |> Option.map( fun cpu -> { cpu with Input = [ int64 v ] } ) }

let setValue x y v g =
    { g with
        Screen = g.Screen |> Map.add (x,y) v
        BallX = if v = 3 then Some x else g.BallX
        PaddleX = if v = 4 then Some x else g.PaddleX }

let updateInput g = 
    match g.BallX, g.PaddleX with
    | Some bx, Some px -> g |> setInput (Math.Clamp( px - bx, -1, 1 ))
    | _,_ ->  g |> setInput 0

let procOutput draw g =
    g.CPU |> Option.map ( fun cpu -> 
        match cpu |> tryTakeOutput 3 with 
        | [-1L; 0L; score ], cpu -> 
            draw 1 0 score
            { g with Score = int score; CPU = Some cpu }
        | [ x; y; t ], cpu -> 
            draw (int x) (int y) t
            { g with CPU = Some cpu }
            |> setValue (int x) (int y) (int t)
        | _ -> { g with CPU = None } )
    |> Option.defaultValue g

let rec run draw g =
    if g.CPU.IsSome then 
        procOutput draw g 
        |> updateInput
        |> run draw
    else g

let tiles = [ " "; "█"; "░"; "╤"; "○" ]

let noRender x y t = ()
let drawConsole xOffset yOffset x y t =
    let origX,origY = Console.CursorLeft, Console.CursorTop
    Console.SetCursorPosition( min (Console.BufferWidth - 1) (xOffset + x), yOffset + y )
    Console.Write( tiles |> List.tryItem (int t) |> Option.defaultValue (t.ToString()) )
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
    |> run noRender
//    |> run (drawConsole 50 (Console.WindowTop+1))
    |> fun g -> g.Score