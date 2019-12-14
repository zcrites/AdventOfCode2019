open System.Diagnostics

let programTimer = new Stopwatch()

let run day part (runProblem:unit->'T) =
    let timer = new Stopwatch ()
    timer.Start ()
    let result = runProblem ()
    timer.Stop ()
    
    let time = float timer.ElapsedMilliseconds / 1000.0
    let total = float programTimer.ElapsedMilliseconds / 1000.0
    printfn "(%.3f) %.3f   Day %d Part %d = %O" total time day part result

[<EntryPoint>]
let main argv =

   programTimer.Start ()
   
   run 1 1 Day1.part1
   run 1 2 Day1.part2
   run 2 1 Day2.part1
   run 2 2 Day2.part2
   run 3 1 Day3.part1
   run 3 2 Day3.part2
   run 4 1 Day4.part1
   run 4 2 Day4.part2
   run 5 1 Day5.part1
   run 5 2 Day5.part2
   run 6 1 Day6.part1
   run 6 2 Day6.part2
   run 7 1 Day7.part1
   run 7 2 Day7.part2
   run 8 1 Day8.part1
   run 8 2 Day8.part2
   run 9 1 Day9.part1
   run 9 2 Day9.part2
   run 10 1 Day10.part1
   run 10 2 Day10.part2
   run 11 1 Day11.part1
   run 11 2 Day11.part2
   run 12 1 Day12.part1
   run 12 2 Day12.part2
   run 13 1 Day13.part1
   run 13 2 Day13.part2

   System.Console.ReadLine () |> ignore
   
   0 
