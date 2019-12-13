module Day2

open System
open System.IO
open Computer

let private input () =
   File.ReadLines "Input/day2.txt"
   |> Seq.map ( fun s -> s.Split ',' )
   |> Seq.concat
   |> Seq.map Int32.Parse
   |> Seq.toArray

let private execInstr op r1 r2 r3 (mem:int array) =
   mem.[r3] <- op mem.[r1] mem.[r2]

let rec private runProgram pc mem = 
   match mem |> Array.item pc with
   | 99 -> mem |> Array.head
   | op ->
      match mem.[pc+1..pc+3] with
      | [|a;b;c|] ->
         execInstr (if op = 1 then (+) else (*)) a b c mem
         runProgram (pc+4) mem
      | _ -> raise ( exn "Invalid opcode params" )

let part1 () =
   let mem = input ()
   mem.[1] <- 12
   mem.[2] <- 2
   runProgram 0 mem 

let part2 () =
   let noun,verb = 
      Seq.allPairs [1..100] [1..100]
      |> Seq.find( fun (noun, verb) ->
         let mem = input ()
         mem.[1] <- noun
         mem.[2] <- verb
         runProgram 0 mem = 19690720 )

   100 * noun + verb
