module Day8

open System
open System.IO

let layerSize = 25 * 6

let input = 
    File.ReadLines "Input/Day8.txt"
    |> Seq.concat
    |> Seq.chunkBySize layerSize

let count char layer =
    layer 
    |> Seq.map( fun d -> if d = char then 1 else 0 ) 
    |> Seq.sum

let part1 () =
    let layer = 
        input
        |> Seq.sortBy ( count '0' )
        |> Seq.head
        
    let ones = count '1' layer
    let twos = count '2' layer
    
    ones * twos

let part2 () =
    let blank = Array.create layerSize '2' |> Array.toSeq

    input
    |> Seq.fold( fun result layer ->
        Seq.zip result layer
        |> Seq.map ( fun (a,b) -> if a = '2' then b else a ) ) blank
    |> Seq.map( fun c -> if c = '1' then '█' else ' ' )
    |> Seq.chunkBySize 25
    |> Seq.map( Seq.toArray >> String )
    |> fun strs -> String.Join( '\n', strs )
    |> sprintf "\n%s"

