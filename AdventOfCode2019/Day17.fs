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
    |> Seq.filter (fun (_,c) -> c <> '\n' )
    |> Map.ofSeq

let neighbors (x,y) = [ (x-1,y); (x+1,y); (x,y-1); (x,y+1) ]

let part1 () =
   scaffoldMap
   |> Map.toSeq
   |> Seq.filter (fun (pos,_) ->
      neighbors pos 
      |> List.exists( fun n -> (Map.tryFind n scaffoldMap) <> Some '#' )
      |> not )
   |> Seq.toList
   |> Seq.map ( fun ((x,y),_) -> x * y )
   |> Seq.sum