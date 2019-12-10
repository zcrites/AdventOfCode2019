module Day5

open System
open System.IO

type Computer =
   { PC : int
     RelOffset : int
     Mem : int64 array 
     Input : int64 list 
     Output : int64 list }

let loadProgram path args =
   { PC = 0
     RelOffset = 0
     Input = args
     Output = [] 
     Mem = 
      File.ReadLines path
      |> Seq.map ( fun s -> s.Split ',' )
      |> Seq.concat
      |> Seq.map Int64.Parse
      |> Seq.toArray }
   |> fun m -> { m with Mem = Array.append m.Mem (Array.create 1000000 (int64 0)) }

let private input args =
    loadProgram "Input/day5.txt" args

let parseOp n =
   n % 100, (n/100) % 10, (n/1000) % 10, (n/10000) % 10

let exInstr m =
   let getParam mode paramIdx (m:Computer) =
      let v = m.Mem.[m.PC+paramIdx]
      match mode with
      | 0 -> m.Mem.[int v]
      | 1 -> v
      | _ -> m.Mem.[m.RelOffset + int32 v]

   let getWriteParam mode paramIdx (m:Computer) =
      let v = int32 m.Mem.[m.PC+paramIdx]
      match mode with
      | 0 | 1 -> v
      | _ -> m.RelOffset + v

   let tmp = m.Mem.[m.PC..m.PC+3]

   let op, m1, m2, m3 = parseOp (int32 m.Mem.[m.PC])
   match op with
   | 99 -> None
   | 1 -> 
      let p1 = getParam m1 1 m
      let p2 = getParam m2 2 m
      let p3 = getWriteParam m3 3 m 
      m.Mem.[p3] <- p1 + p2
      Some { m with PC = m.PC + 4 }
   | 2 -> 
      let p1 = getParam m1 1 m
      let p2 = getParam m2 2 m
      let p3 = getWriteParam m3 3 m 
      m.Mem.[p3] <- p1 * p2
      Some { m with PC = m.PC + 4 }
   | 3 -> 
      match m.Input with
      | v::tail ->
         let p1 = getWriteParam m1 1 m
         m.Mem.[p1] <- v
         Some { m with PC = m.PC + 2; Input = tail }
      | _ -> failwith "ERR: NO INPUT"
   | 4 -> 
      let v = getParam m1 1 m
      Some { m with PC = m.PC + 2; Output = v::m.Output }
   | 5 -> 
      let p1 = getParam m1 1 m
      let p2 = getParam m2 2 m
      if int32 p1 <> 0
      then Some { m with PC = int32 p2 }
      else Some { m with PC = m.PC + 3 }
   | 6 -> 
      let p1 = getParam m1 1 m
      let p2 = getParam m2 2 m
      if int32 p1 = 0
      then Some { m with PC = int32 p2 }
      else Some { m with PC = m.PC + 3 }
   | 7 ->
      let p1 = getParam m1 1 m
      let p2 = getParam m2 2 m
      let p3 = getWriteParam m3 3 m
      m.Mem.[p3] <- int64 <| if p1 < p2 then 1 else 0
      Some { m with PC = m.PC + 4 }
   | 8 ->
      let p1 = getParam m1 1 m
      let p2 = getParam m2 2 m
      let p3 = getWriteParam m3 3 m
      m.Mem.[p3] <- int64 <| if p1 = p2 then 1 else 0
      Some { m with PC = m.PC + 4 }
   | 9 ->
      let p1 = getParam m1 1 m
      Some { m with PC = m.PC + 2; RelOffset = m.RelOffset + int32 p1 }
   | _ -> None

let runProgram m =
   let rec loop m =
      match exInstr m with
      | Some n -> loop n
      | None -> m
   
   let halted = loop m
   halted.Output |> List.head

let part1 () =
   input [ 1L ]
   |> runProgram
   
let part2 () =
   input [ 5L ]
   |> runProgram