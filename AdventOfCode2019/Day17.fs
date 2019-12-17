module Day17

open System
open Computer

let scaffoldMap = 
    loadProgram "Input/day17.txt"
    |> outputSeq
    |> Seq.map (fun (v,_) -> char v)
    |> Seq.scan( fun ((x,y),last) c -> 
       if last = '\n' 
       then (0, y+1), c
       else (x+1, y), c ) ((0,-1),'\n')
    |> Seq.filter (fun (_,c) -> c <> '\n' && c <> '.' )
    |> Map.ofSeq

let neighbors (x,y) = [ (x-1,y); (x+1,y); (x,y-1); (x,y+1) ]

let isIntersection pos =
    neighbors pos 
    |> List.exists( fun n -> (Map.tryFind n scaffoldMap) <> Some '#' )
    |> not 

let part1 () =
   Util.printMap (Option.defaultValue ' ') scaffoldMap |> printfn "%s"

   scaffoldMap
   |> Map.toSeq
   |> Seq.filter (fun (pos,_) -> isIntersection pos )
   |> Seq.toList
   |> Seq.map ( fun ((x,y),_) -> x * y )
   |> Seq.sum

let toInputList (input: string) =
    input 
    |> sprintf "%s\n"
    |> Seq.map( fun c -> int64 c )
    |> Seq.toList

let flushOutput cpu =
    cpu.Output 
    |> Seq.rev 
    |> Seq.map( fun v -> char v ) 
    |> Seq.toArray |> String |> printf "%s"
    { cpu with Output = [] }

let rec collectInputDiffs result cpu =
    if cpu.Input.Length = 0 then cpu,(result |> List.rev)
    else
        match tick cpu with
        | Some newCpu when cpu.Input.Length <> newCpu.Input.Length ->
            let result = (cpu,newCpu)::result
            collectInputDiffs result newCpu
        | Some cpu -> collectInputDiffs result cpu
        | None -> failwith "program halted"

// Address of sequence A = 1194

type Ctx = { Collected : int64; Last : Computer; Points : (int*int) list; StartPos : (int*int); LastPos : (int*int); Set : bool; LastOutput : int64 }


let xPosAddr = 576
let yPosAddr = 577
let rotAddr = 578

let collectedAddr = 438

let opUpdatesPosition cpu =
    if opcode cpu <> 1 then false
    else 
        let addr = cpu |> getParamAddr 3
        if addr = yPosAddr
        then (addr = addr)
        else false

let setPosition (x,y) cpu = 
    cpu 
    |> write xPosAddr (int64 x)
    |> write yPosAddr (int64 y)

let part2 () =
    let start = 
        loadProgram "Input/day17.txt"
        |> write 0 2L

    let p2 =
        { start with Input = "A,A,A,A\n9,9,9,9,9,9,9,9,9,9\nR\nR\nn" |> toInputList }

    let p3,r3 = collectInputDiffs [] p2

    let startPos = scaffoldMap |> Map.toSeq |> Seq.find( fun (_,c) -> c = '^') |> fst

    let rec printNextOutputLine (r:Ctx) cpu =
        match tick cpu with
        | Some newCpu -> 

            let newCpu,r =
                if opUpdatesPosition cpu then
                    let diff = memDiff r.Last cpu

                    match r.Points with
                    | p::ps ->
                        newCpu |> setPosition p,
                        { r with
                            Points = ps
                            LastPos = p
                            Last = newCpu }
                    | _ -> newCpu |> setPosition r.StartPos, { r with Last = newCpu }
                else 
                    newCpu, r


            //let xPos = newCpu |> read xPosAddr
            //let yPos = newCpu |> read yPosAddr
            //let rot = newCpu |> read rotAddr

            let lastNewline =
                if newCpu.Output |> List.tryHead = (Some 10L) then 
                    printf "NEWLIEN"
                    newCpu
                else 
                    r.Last
            let r = { r with Collected = newCpu |> read collectedAddr; LastOutput = newCpu.Output |> List.tryHead |> Option.defaultValue r.LastOutput }
            let newCpu = newCpu |> flushOutput
            printNextOutputLine r newCpu
        | _ -> cpu,r

    let pts =
        scaffoldMap 
        |> Map.toList
        |> List.map fst
        |> List.append (
            scaffoldMap
            |> Map.toList
            |> List.filter (fun (pos,_) -> isIntersection pos )
            |> List.map fst )

    let result,r = 
        let p4 = p3 |> flushOutput
        printNextOutputLine { Collected = 0L; Last = p4; StartPos = startPos; Set = false; Points = pts; LastPos = (0,0); LastOutput = 0L } p4

    result
