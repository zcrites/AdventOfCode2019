module Day10

open System.IO

type Asteroid = { X:int; Y:int }

let input = 
   File.ReadAllLines "Input/Day10.txt"
   |> Seq.mapi( fun y line -> 
      line 
      |> Seq.mapi( fun x c -> if c = '#'then Some { X = x; Y = y } else None) ) 
   |> Seq.concat
   |> Seq.choose id
   
let private isBetween a b x =
   let l,r = if a < b then a,b else b,a
   l <= x && x <= r

let isAsteriodBetween a b o =
   if isBetween a.X b.X o.X && isBetween a.Y b.Y o.Y then
      if o = a || o = b then false
      else if a.Y = b.Y && isBetween a.X b.X o.X then true
      else if a.Y = o.Y then false
      else
         let r1 = (float o.X - float a.X) / (float o.Y - float a.Y) 
         let r2 =  (float b.X - float a.X) / (float b.Y - float a.Y)
         r1 = r2
   else false

let checkVisibility (a:Asteroid) (b:Asteroid) others =
   others
   |> Seq.tryFind( isAsteriodBetween a b )
   |> Option.isNone

let increment key map =
   match Map.tryFind key map with
   | Some v -> Map.add key (v+1) map
   | None ->  Map.add key 1 map

let part1 () =
   Seq.allPairs input input
   |> Seq.filter( fun (a,b) -> a < b )
   |> Seq.filter( fun (a,b) -> checkVisibility a b input )
   |> Seq.fold ( fun result (a,b) -> result |> increment a |> increment b ) Map.empty
   |> Map.toSeq
   |> Seq.maxBy ( fun (k,v) -> v )
   |> snd

