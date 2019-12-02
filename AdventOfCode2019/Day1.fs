module Day1

open System
open System.IO

let private requiredFuel mass = mass / 3 - 2  

let rec private totalFuel fuelMass = 
   let moreFuel = requiredFuel fuelMass
   if moreFuel > 0
   then fuelMass + totalFuel moreFuel
   else fuelMass

let private input =
   File.ReadLines "Input/day1.txt"
   |> Seq.map Int32.Parse

let part1 () =  
   input
   |> Seq.map requiredFuel
   |> Seq.sum

let part2 () = 
   input
   |> Seq.map requiredFuel
   |> Seq.map totalFuel
   |> Seq.sum