module Day6

open System.IO

let private orbits =
   File.ReadLines "Input/day6.txt"
   |> Seq.map (fun s -> s.Split ')')
   |> Seq.fold ( fun orbits line -> 
      match line with 
      | [|planet;moon|] -> orbits |> Map.add moon planet
      | _ -> orbits ) Map.empty

let rec private countOrbits moon orbits =
   match orbits |> Map.tryFind moon with
   | Some planet -> 1 + countOrbits planet orbits
   | None -> 0

let rec private trace result moon orbits =
   match orbits |> Map.tryFind moon with
   | Some planet -> trace (planet::result) planet orbits
   | None -> result

let part1 () =
   orbits
   |> Map.toSeq
   |> Seq.map( fun (moon,_) -> countOrbits moon orbits )
   |> Seq.sum
   
let part2 () =
   let yourTrace = trace [] "YOU" orbits
   let sanTrace = trace [] "SAN" orbits

   let commonLength =
      Seq.zip yourTrace sanTrace 
      |> Seq.findIndex (fun (a,b) -> a <> b)
      
   (yourTrace.Length - commonLength) + (sanTrace.Length - commonLength)

