module Day5

open System
open System.IO

type Computer =
   { PC : int
     Mem : int array 
     Input : int list 
     Output : int list }

let private input () =
   { PC = 0
     Input = [ 1 ]
     Output = [] 
     Mem = 
      File.ReadLines "Input/day5.txt"
      |> Seq.map ( fun s -> s.Split ',' )
      |> Seq.concat
      |> Seq.map Int32.Parse
      |> Seq.toArray }

let parseOp n =
   n % 100, (n/100) % 10 = 1, (n/1000) % 10 = 1, (n/10000) % 10 = 1

let exInstr m =
   let getParam immediate v (mem:int array) =
      if immediate then v else mem.[v]

   let op, m1, m2, m3 = parseOp m.Mem.[m.PC]
   match op with
   | 99 -> None
   | 1 -> 
      let p1 = getParam m1 m.Mem.[m.PC+1] m.Mem
      let p2 = getParam m2 m.Mem.[m.PC+2] m.Mem
      let addr = m.Mem.[m.PC+3] 
      m.Mem.[addr] <- p1 + p2
      Some { m with PC = m.PC + 4 }
   | 2 -> 
      let p1 = getParam m1 m.Mem.[m.PC+1] m.Mem
      let p2 = getParam m2 m.Mem.[m.PC+2] m.Mem
      let addr = m.Mem.[m.PC+3] 
      m.Mem.[addr] <- p1 * p2
      Some { m with PC = m.PC + 4 }
   | 3 -> 
      match m.Input with
      | v::tail ->
         let addr = m.Mem.[m.PC + 1]
         m.Mem.[addr] <- v
         Some { m with PC = m.PC + 2; Input = tail }
      | _ -> failwith "ERR: NO INPUT"
   | 4 -> 
      let v = getParam m1 m.Mem.[m.PC+1] m.Mem
      Some { m with PC = m.PC + 2; Output = v::m.Output }
   | _ -> None

let runProgram m =
   let rec loop m =
      match exInstr m with
      | Some n -> loop n
      | None -> m
   
   let halted = loop m
   halted.Output |> List.head

let part1 () =
   input ()
   |> runProgram
