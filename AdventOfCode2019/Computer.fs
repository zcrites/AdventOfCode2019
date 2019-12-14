module Computer

open System
open System.IO

type Computer =
   { PC : int
     RelOffset : int
     Mem : Map<int,int64>
     Input : int64 list 
     Output : int64 list }

let read addr m =
    m.Mem |> Map.tryFind addr |> Option.defaultValue 0L
    
let write addr v m =
    { m with Mem = m.Mem |> Map.add addr v }

let private jumpTo addr m = 
    { m with PC = addr }

let private incrementPC v m = 
    { m with PC = m.PC + v }

let private opcode m = 
    read m.PC m % 100L |> int

let private getParamAddr i m =
    let paramMode = ( read m.PC m / ( pown 10L i  * 10L ) ) % 10L |> int

    match paramMode with
    | 0 -> read (m.PC+i) m |> int
    | 1 -> m.PC+i 
    | 2 | _ -> (read (m.PC+i) m |> int) + m.RelOffset

let private binaryOp fn m = 
    let p1 = m |> getParamAddr 1
    let p2 = m |> getParamAddr 2
    let p3 = m |> getParamAddr 3
    let r = fn (m |> read p1) (m |> read p2)
    m |> write p3 r |> incrementPC 4
    
let private jumpIf predicate m =
    let p1 = m |> getParamAddr 1
    let p2 = m |> getParamAddr 2

    if m |> read p1 |> predicate
    then m |> jumpTo (m |> read p2 |> int)
    else m |> incrementPC 3

let private exAdd = binaryOp (+)
let private exMult = binaryOp (*)
let private exLessThan = binaryOp (fun a b -> if a < b then 1L else 0L)
let private exEquals = binaryOp (fun a b -> if a = b then 1L else 0L)
let private jumpIfTrue m = jumpIf ((<>) 0L) m
let private jumpIfFalse m = jumpIf ((=) 0L) m

let private offsetRelativeBase m =
    let p1 = m |> getParamAddr 1
    { m with RelOffset = m.RelOffset + (m |> read p1 |> int) }
    |> incrementPC 2

let private exInput m =
    match m.Input with
    | v::vs -> 
        let p1 = m |> getParamAddr 1
        { m with Input = vs }
        |> write p1 v
        |> incrementPC 2
    | _ -> failwith "No input"
    
let private exOutput m =
    let p1 = m |> getParamAddr 1
    { m with Output = (m |> read p1)::m.Output }
    |> incrementPC 2

let tick m =
    match opcode m with
    | 99 -> None
    | 1 -> Some (exAdd m)
    | 2 -> Some (exMult m)
    | 3 -> Some (exInput m)
    | 4 -> Some (exOutput m)
    | 5 -> Some (jumpIfTrue m)
    | 6 -> Some (jumpIfFalse m)
    | 7 -> Some (exLessThan m)
    | 8 -> Some (exEquals m)
    | 9 -> Some (offsetRelativeBase m)
    | _ -> None

let rec run m =
    match tick m with
    | Some update -> run update
    | None -> m

let input v m =
    { m with Input = List.append m.Input [v] }

let rec nextOutput cpu =
   match tick cpu with
   | Some next when next.Output.Length = 1 -> Some (next.Output.Head, { next with Output = [] })
   | Some next -> nextOutput next
   | None -> None

let loadProgram path = 
    { PC = 0
      RelOffset = 0
      Input = []
      Output = []
      Mem = 
        File.ReadLines path 
        |> Seq.map ( fun s -> s.Split ',' )
        |> Seq.concat
        |> Seq.map Int64.Parse
        |> Seq.indexed
        |> Seq.fold ( fun mem (i,v) -> mem |> Map.add i v ) Map.empty }