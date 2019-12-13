module Day5

open Computer

let part1 () = 
   loadProgram "Input/Day5.txt"
   |> input 1L
   |> run
   |> fun c -> c.Output.Head

let part2 () =
   loadProgram "Input/Day5.txt"
   |> input 5L
   |> run
   |> fun c -> c.Output.Head