module Day9

open Computer

let part1 () =
   loadProgram "Input/Day9.txt"
   |> input 1L
   |> run
   |> fun c -> c.Output.Head

let part2 () =
   loadProgram "Input/Day9.txt"
   |> input 2L
   |> run
   |> fun c -> c.Output.Head
