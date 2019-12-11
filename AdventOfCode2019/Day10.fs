module Day10

open System.IO

type Asteroid = { X:int; Y:int }

let input = 
   File.ReadAllLines "Input/Day10.txt"
   |> Seq.mapi( fun y line -> 
      line |> Seq.mapi( fun x c -> if c = '#' then Some { X = x; Y = y } else None) ) 
   |> Seq.concat
   |> Seq.choose id
   |> Seq.toList
   
let isBetween a b x =
   let l,r = if a < b then a,b else b,a
   l <= x && x <= r

let isAsteriodBetween a b o =
   if not (isBetween a.X b.X o.X && isBetween a.Y b.Y o.Y) then false
   elif o = a || o = b then false
   elif a.Y = b.Y && isBetween a.X b.X o.X then true
   elif a.Y = o.Y then false
   else
      let r1 = (float o.X - float a.X) / (float o.Y - float a.Y) 
      let r2 =  (float b.X - float a.X) / (float b.Y - float a.Y)
      r1 = r2

let checkVisibility (a:Asteroid) others (b:Asteroid) =
   others
   |> Seq.tryFind( isAsteriodBetween a b )
   |> Option.isNone

let increment key map =
   match Map.tryFind key map with
   | Some v -> Map.add key (v+1) map
   | None ->  Map.add key 1 map

let station,detected =
   Seq.allPairs input input
   |> Seq.filter( fun (a,b) -> a < b )
   |> Seq.filter( fun (a,b) -> checkVisibility a input b  )
   |> Seq.fold ( fun result (a,b) -> result |> increment a |> increment b ) Map.empty
   |> Map.toSeq
   |> Seq.maxBy ( fun (_,v) -> v )

let rec vaporizeFrom loc asteroids =
   let dead,alive = 
      asteroids |> List.partition( checkVisibility loc asteroids )

   if List.length alive > 0
   then List.append dead (vaporizeFrom loc alive)
   else dead

let part1 () = detected

let part2 () =
    input 
    |> List.sortBy( fun v -> 
        let x,y = v.X - station.X, v.Y - station.Y 
        atan2 (float -x) (float y) ) 
    |> vaporizeFrom station  
    |> List.item 199
    |> fun a -> a.X * 100 + a.Y