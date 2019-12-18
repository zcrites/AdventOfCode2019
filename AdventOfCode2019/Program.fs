open System.Diagnostics

let programTimer = new Stopwatch()

let run day part (runProblem:unit->'T) =
    let timer = new Stopwatch ()
    timer.Start ()
    let result = runProblem ()
    timer.Stop ()
    
    let time = float timer.ElapsedMilliseconds / 1000.0
    let total = float programTimer.ElapsedMilliseconds / 1000.0
    printfn "(%.3fs) %.3fs   Day %d Part %d = %O" total time day part result

[<EntryPoint>]
let main _ =

   programTimer.Start ()

   [ 
       Day1.part1 >> sprintf "%d"
       Day1.part2 >> sprintf "%d"
       Day2.part1 >> sprintf "%d"
       Day2.part2 >> sprintf "%d"
       Day3.part1 >> sprintf "%d"
       Day3.part2 >> sprintf "%d"
       Day4.part1 >> sprintf "%d"
       Day4.part2 >> sprintf "%d"
       Day5.part1 >> sprintf "%d"
       Day5.part2 >> sprintf "%d"
       Day6.part1 >> sprintf "%d"
       Day6.part2 >> sprintf "%d"
       Day7.part1 >> sprintf "%d"
       Day7.part2 >> sprintf "%d"
       Day8.part1 >> sprintf "%d"
       Day8.part2
       Day9.part1 >> sprintf "%d"
       Day9.part2 >> sprintf "%d"
       Day10.part1 >> sprintf "%d"
       Day10.part2 >> sprintf "%d"
       Day11.part1 >> sprintf "%d"
       Day11.part2
       Day12.part1 >> sprintf "%d"
       Day12.part2 >> sprintf "%d"
       Day13.part1 >> sprintf "%d"
       Day13.part2 >> sprintf "%d"
       Day14.part1 >> sprintf "%d"
       Day14.part2 >> sprintf "%d" 
       Day15.part1 >> sprintf "%d"
       Day15.part2 >> sprintf "%d" 
       Day16.part1
       Day16.part2
       Day17.part1 >> sprintf "%d" 
       Day17.part2 >> sprintf "%d" 
   ]
   |> List.indexed 
   |> List.iter (fun (i, part) -> run (1 + i / 2) (1 + i % 2) part)


   System.Console.ReadLine () |> ignore
   
   0 
