[<EntryPoint>]
let main argv =

   [ sprintf "Day 1 Part 1 = %d" <| Day1.part1 () 
     sprintf "Day 1 Part 2 = %d" <| Day1.part2 ()
     sprintf "Day 2 Part 1 = %d" <| Day2.part1 ()
     sprintf "Day 2 Part 2 = %d" <| Day2.part2 () 
     sprintf "Day 3 Part 1 = %d" <| Day3.part1 () 
     sprintf "Day 3 Part 2 = %d" <| Day3.part2 ()
     sprintf "Day 4 Part 1 = %d" <| Day4.part1 () 
     sprintf "Day 4 Part 2 = %d" <| Day4.part2 ()
     sprintf "Day 5 Part 1 = %d" <| Day5.part1 () 
     sprintf "Day 5 Part 2 = %d" <| Day5.part2 ()
     sprintf "Day 6 Part 1 = %d" <| Day6.part1 () 
     sprintf "Day 6 Part 2 = %d" <| Day6.part2 () 
     sprintf "Day 7 Part 1 = %d" <| Day7.part1 () 
     sprintf "Day 7 Part 2 = %d" <| Day7.part2 () ]
   |> Seq.iter (printfn "%s")

   System.Console.ReadLine () |> ignore
   
   0 
