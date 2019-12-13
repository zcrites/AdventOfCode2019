module Day11

open System
open Computer

type Point = 
   { X:int; Y:int }

type Robot =
   { Position:Point; Direction:Point; Paint:Map<Point,int>; CPU:Computer option }
    
let rec nextOutput cpu =
   match tick cpu with
   | Some next when next.Output.Length = 1 -> Some (next.Output.Head, { next with Output = [] })
   | Some next -> nextOutput next
   | None -> None

let handleOutput callback bot =
   bot.CPU
   |> Option.map nextOutput 
   |> Option.flatten
   |> Option.map (fun (output,cpu) -> callback output { bot with CPU = Some cpu } )
   |> Option.defaultValue { bot with CPU = None }

let paintPosition color bot =
   { bot with Paint = bot.Paint |> Map.add bot.Position (int color) }

let turn angle bot =
   let direction = 
      if angle = 0L 
      then { X = -bot.Direction.Y; Y = bot.Direction.X }
      else { X = bot.Direction.Y; Y = -bot.Direction.X } 
   { bot with
      Position = { X = bot.Position.X + direction.X; Y = bot.Position.Y + direction.Y }
      Direction = direction } 
    
let input v bot =
   let cpuInput v cpu = { cpu with Input = [ v ] }
   { bot with CPU = bot.CPU |> Option.map (cpuInput v) }

let readColor bot =
   let color =
      bot.Paint 
      |> Map.tryFind bot.Position 
      |> Option.defaultValue 0
   input (int64 color) bot

let rec run bot =
   if Option.isNone bot.CPU 
   then bot
   else 
      bot
      |> readColor
      |> handleOutput paintPosition
      |> handleOutput turn
      |> run
      
let robot () = 
   { Position = { X = 0; Y = 0 }
     Direction = { X = 0; Y = 1 }
     Paint = Map.empty
     CPU = Some <| loadProgram "Input/Day11.txt" }

let part1 () =
   let halted = robot () |> run
   halted.Paint |> Map.count

let part2 () =
   let halted =
      robot ()
      |> paintPosition 1L
      |> run
   
   let (minx,maxx,miny,maxy) =
      halted.Paint 
      |> Map.toSeq
      |> Seq.fold( fun (minx,maxx,miny,maxy) ({X=x;Y=y},_) -> 
         (min minx x, max maxx x, min miny y, max maxy y) ) (0,0,0,0)

   [maxy..(-1)..miny] 
   |> Seq.map( fun y -> 
      [minx..maxx] 
      |> Seq.map( fun x -> halted.Paint |> Map.tryFind {X=x;Y=y} |> Option.defaultValue 0 ) 
      |> Seq.map( fun v -> if v = 0 then ' ' else '█' ) 
      |> Seq.append ['\n'] )
   |> Seq.concat |> Seq.toArray |> String