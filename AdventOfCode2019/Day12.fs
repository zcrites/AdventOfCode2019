module Day12

open System
open System.IO

type Planet = { Pos : int list; Vel : int list }

let inputPlanets = 
   File.ReadLines "Input/Day12.txt"
   |> Seq.map( fun line -> line.Replace( "<x=", "").Replace( "y=","").Replace( "z=","").Replace( ">","") )
   |> Seq.map( fun line -> line.Split(',') |> Seq.map Int32.Parse |> Seq.toList )
   |> Seq.map( fun pos -> { Pos = pos; Vel = [ 0; 0; 0 ] } )
   |> Seq.toList

let gravity v1 v2 =
    Math.Clamp( v2 - v1, -1, 1 )

let zipMap fn a b =
   List.zip a b |> List.map( fun (a,b) -> fn a b )

let applyGravity others planet =
   others
   |> Seq.fold( fun p other ->
      let g = zipMap gravity p.Pos other.Pos
      let vel = p.Vel |> zipMap (+) g
      { Pos = p.Pos; Vel = vel } ) planet

let updatePosition p =
   { p with Pos = p.Pos |> zipMap (+) p.Vel }

let step planets = 
   planets 
   |> List.map( applyGravity planets ) 
   |> List.map( updatePosition )

let energy planets =
   planets 
   |> List.map ( fun p ->
      let potential = p.Pos |> List.map abs |> List.sum 
      let kinetic = p.Vel |> List.map abs |> List.sum 
      kinetic * potential )
   |> List.sum

let extract idx p =
   { Pos = [ p.Pos.[idx] ]; Vel = [ p.Vel.[idx] ] }

let rec gcd x y =
   if y = 0L then x else gcd y ( x % y )

let lcm x y = 
   x * y / ( gcd x y )

let findPeriod planets =
   let rec loop current n =
      if current = planets 
      then n
      else loop (step current) (n+1)
   loop (step planets) 1
    
let part1 () =
   [1..1000]
   |> List.fold( fun planets _ -> step planets ) inputPlanets
   |> energy

let part2 () =
   [0..2]
   |> List.map ( fun i -> 
      inputPlanets 
      |> List.map (extract i) 
      |> findPeriod 
      |> int64 )
   |> List.reduce lcm