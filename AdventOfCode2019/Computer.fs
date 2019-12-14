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

let private jumpTo addr m = { m with PC = addr }
let private incrementPC v m = { m with PC = m.PC + v }
let private setInput v m = { m with Input = v }
let private setOutput v m = { m with Output = v }

let private opcode m = 
    read m.PC m % 100L |> int

let private getParamAddr i m =
    let paramMode = ( read m.PC m / ( pown 10L i  * 10L ) ) % 10L |> int
    match paramMode with
    | 0 -> read (m.PC+i) m |> int
    | 1 -> m.PC+i 
    | 2 | _ -> (read (m.PC+i) m |> int) + m.RelOffset

let private binaryOp op addr1 addr2 addr3 m =
    let result = op (m |> read addr1) (m |> read addr2)
    m |> write addr3 result |> incrementPC 4

let private jumpIf predicate pAddr1 pAddr2 m =
    if m |> read pAddr1 |> predicate
    then m |> jumpTo (m |> read pAddr2 |> int)
    else m |> incrementPC 3

let private add = binaryOp (+)
let private mult = binaryOp (*)
let private lt = binaryOp (fun a b -> if a < b then 1L else 0L)
let private eq = binaryOp (fun a b -> if a = b then 1L else 0L)
let private jumpTrue = jumpIf ((<>)0L)
let private jumpFalse = jumpIf ((=)0L)

let private readInput pAddr1 m =
    match m.Input with
    | v::vs -> m |> write pAddr1 v |> setInput vs |> incrementPC 2
    | _ -> failwith "No input"

let private writeOutput pAddr1 m =
    m |> setOutput ((m |> read pAddr1)::m.Output) |> incrementPC 2

let private offsetBase pAddr1 m =
    { m with RelOffset = m.RelOffset + (m |> read pAddr1 |> int) } |> incrementPC 2

let tick m =
    match opcode m with
    | op when 0 < op && op < 10 ->
        let pAddr1 = m |> getParamAddr 1
        match op with
        | 1 | 2 | 7 | 8 ->
            let pAddr2 = m |> getParamAddr 2 
            let pAddr3 = m |> getParamAddr 3
            match op with 
            | 1 -> add pAddr1 pAddr2 pAddr3 m
            | 2 -> mult pAddr1 pAddr2 pAddr3 m
            | 7 -> lt pAddr1 pAddr2 pAddr3 m
            | 8 | _ -> eq pAddr1 pAddr2 pAddr3 m
        | 5 | 6 ->
            let pAddr2 = m |> getParamAddr 2
            if op = 5
            then jumpTrue pAddr1 pAddr2 m
            else jumpFalse pAddr1 pAddr2 m
        | 3 -> readInput pAddr1 m
        | 4 -> writeOutput pAddr1 m
        | 9 | _ -> offsetBase pAddr1 m
        |> Some
    | 99 | _ -> None

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

let rec outputSeq cpu = seq {
    match nextOutput cpu with
    | Some (v,next) -> 
        yield (v,next)
        yield! outputSeq next
    | None -> () }

let tryTakeOutput n cpu =
    outputSeq cpu 
    |> Seq.truncate n
    |> Seq.fold ( fun (result,_) (v,cpu) ->  (v::result,cpu) ) ([],cpu)
    |> fun (vs,cpu) -> (vs |> List.rev,cpu) 

let loadProgram path = 
    { PC = 0
      RelOffset = 0
      Input = []
      Output = []
      Mem = 
        File.ReadAllText path 
        |> fun s -> s.Split ',' 
        |> Seq.map Int64.Parse
        |> Seq.indexed
        |> Seq.fold ( fun mem (i,v) -> mem |> Map.add i v ) Map.empty }