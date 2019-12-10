module Day9

open Day5

let part1 () =
   loadProgram "Input/Day9.txt" [ 1L ]
   |> runProgram

let part2 () =
   loadProgram "Input/Day9.txt" [ 2L ]
   |> runProgram

