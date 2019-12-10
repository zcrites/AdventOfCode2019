module Day7

open Day5

let runAmp phase input =
    loadProgram "Input/Day7.txt" [ phase; input ]
    |> runProgram

let rec check input (phases:int list) =
    match phases with
    | next::rest ->
        let nextInput = runAmp (int64 next) input
        check nextInput rest
    | _ -> input

let rec interleave x = function
    | [] -> [[x]]
    | y::ys -> 
        [ yield x::y::ys
          for zs in interleave x ys do
            yield! [y::zs]]
            
let rec permutations = function
    | [] -> [[]]
    | x::xs -> List.concat (List.map (interleave x) (permutations xs))

let part1 () =
    permutations [0..4]
    |> List.map ( fun phases -> check 0L phases )
    |> List.max

let runFeeback phases = 
    let rec run (units:Computer list) =
        match units with
        | current::(next::rest) ->
            match Day5.exInstr current with
            | Some update ->
                match update.Output with
                | [ v ] -> 
                    let next = { next with Input = List.append next.Input [ v ] }
                    run (next::(List.append rest [ { update with Output = [] } ]))
                | _ -> run (update::(next::rest))
            | None -> 
                match current.Input with
                | [ v ] -> int32 v
                | _ -> 0
        | _ -> 0
        
    phases 
    |> List.mapi ( fun i p -> loadProgram "Input/Day7.txt" [ yield (int64 p);  if i = 0 then yield 0L ] )
    |> run

let part2 () =
    permutations [5..9]
    |> List.map ( fun phases -> runFeeback phases )
    |> List.max
