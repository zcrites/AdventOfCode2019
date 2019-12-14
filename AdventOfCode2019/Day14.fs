module Day14

open System.IO

let split (c:string) (str:string) = str.Split( c )

type Reagent = { Chem : string; Quantity : int64 }
type Production = { RequiredOre : int64; Stockpile : Map<string,int64> }

let reactions =
    let parseReagent (e:string) =
        match split " " (e.Trim()) with
        | [| n; name |] -> { Chem = name; Quantity = int64 n }
        | _ -> failwith "parseReagent"

    File.ReadLines "Input/day14.txt"
    |> Seq.map ( fun line ->
        match line.Split( "=>") with
        | [| input; output; |] -> 
            input |> split "," |> Seq.map parseReagent |> Seq.toList, 
            parseReagent output
        | _ -> failwith "reactions" )
    |> Seq.fold( fun map (input,output) -> map |> Map.add output.Chem (input,output) ) Map.empty  

let stockpiled chem p =
    p.Stockpile |> Map.tryFind chem |> Option.defaultValue 0L

let add chem v p = 
    { p with Stockpile = p.Stockpile |> Map.add chem (v + (stockpiled chem p)) }

let take chem v p =
    { p with Stockpile = p.Stockpile |> Map.add chem ((stockpiled chem p) - v) }

let mineOre v p = 
    { p with RequiredOre = p.RequiredOre + int64 v } |> add "ORE" v

let rec produce chem quantity state =
    if chem = "ORE" then state |> mineOre quantity
    elif quantity <= (state |> stockpiled chem) then state
    else    
        let input,output = reactions |> Map.find chem
        let needed = quantity - (state |> stockpiled chem)
        let scale = System.MathF.Ceiling ( float32 needed / float32 output.Quantity ) |> int64

        input 
        |> List.fold ( fun state input ->
            state
            |> produce input.Chem (input.Quantity * scale)
            |> take input.Chem (input.Quantity * scale) ) state
        |> add output.Chem (output.Quantity * scale)

let newProduction = { RequiredOre = 0L; Stockpile = Map.empty }

let rec findLastWhen min max condition =
    let t = min + ( max - min ) / 2L
    if condition t 
    then if min = max then t else findLastWhen (t+1L) max condition
    else if min = max then t-1L else findLastWhen min t condition

let part1 () =
    newProduction
    |> produce "FUEL" 1L 
    |> fun r -> r.RequiredOre

let part2 () =
    let onetrillion = 1000000000000L

    findLastWhen 1L onetrillion <| fun n ->
        newProduction 
        |> produce "FUEL" n 
        |> fun r -> r.RequiredOre < onetrillion