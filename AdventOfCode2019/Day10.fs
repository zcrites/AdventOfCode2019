module Day10

open System.IO

type Asteroid = { X:int; Y:int }

let asteroidSet = 
   File.ReadAllLines "Input/Day10.txt"
   |> Seq.mapi( fun y line -> 
      line |> Seq.mapi( fun x c -> if c = '#' then Some { X = x; Y = y } else None) ) 
   |> Seq.concat
   |> Seq.choose id
   |> Set.ofSeq
   
let rec gcd x y =
   if y = 0 then x else gcd y ( x % y )

let detects a others b =
   let dx,dy = b.X - a.X, b.Y - a.Y
   let d = gcd dx dy

   if d < -1 then [-1..-1..d+1] else [1..d-1]
   |> Seq.tryFind ( fun i ->
      let x = a.X + i * dx / d
      let y = a.Y + i * dy / d
      Set.contains { X = x; Y = y } others )
   |> Option.isNone

let increment key map =
   match Map.tryFind key map with
   | Some v -> Map.add key (v+1) map
   | None -> Map.add key 1 map

let station,detected =
   Seq.allPairs asteroidSet asteroidSet
   |> Seq.filter ( fun (a,b) -> a < b )
   |> Seq.filter ( fun (a,b) -> detects a asteroidSet b  )
   |> Seq.fold ( fun result (a,b) -> result |> increment a |> increment b ) Map.empty
   |> Map.toSeq
   |> Seq.maxBy ( fun (_,v) -> v )

let rec vaporizeFrom loc asteroids =
   let dead,alive = 
      asteroids |> List.partition ( detects loc (Set.ofSeq asteroids) )

   if List.length alive > 0
   then List.append dead (vaporizeFrom loc alive)
   else dead

let part1 () = detected

let part2 () =
   asteroidSet 
   |> Set.toList
   |> List.sortBy( fun v -> 
      let x,y = v.X - station.X, v.Y - station.Y 
      atan2 (float -x) (float y) ) 
   |> vaporizeFrom station  
   |> List.item 199
   |> fun a -> a.X * 100 + a.Y