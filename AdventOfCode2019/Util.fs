module Util

open System

let printMap getChar map =
    let (minx,maxx,miny,maxy) =
       map
       |> Map.toSeq
       |> Seq.fold( fun (minx,maxx,miny,maxy) ((x,y),_) -> 
          (min minx x, max maxx x, min miny y, max maxy y) ) (0,0,0,0)

    [maxy..(-1)..miny] 
    |> Seq.map( fun y -> 
       [minx..maxx] 
       |> Seq.map( fun x -> map |> Map.tryFind (x,y) |> getChar ) 
       |> Seq.append ['\n'] )
    |> Seq.concat |> Seq.toArray |> String