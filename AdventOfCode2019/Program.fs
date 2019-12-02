[<EntryPoint>]
let main argv =

   [ sprintf "Day 1 Part 1 = %d" <| Day1.part1 () 
     sprintf "Day 1 Part 2 = %d" <| Day1.part2 ()
     sprintf "Day 2 Part 1 = %d" <| Day2.part1 ()
     sprintf "Day 2 Part 2 = %d" <| Day2.part2 () ]
   |> Seq.iter (printfn "%s")

   System.Console.ReadLine () |> ignore
   0 
