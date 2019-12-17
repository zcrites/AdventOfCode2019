module Day16

open System
open System.IO

let input =
   File.ReadAllText "Input/day16.txt"
   |> Seq.map (fun c -> int c - int '0')
   |> Seq.filter (fun c -> 0 <= c && c < 10)
   |> Seq.toArray

let input100 =
   Seq.replicate 10000 input
   |> Seq.concat
   |> Seq.toArray

let fft (signal:int array) =
   let sums =
      (Array.scan (+) 0 signal) 

   [1..signal.Length]
   |> Seq.map( fun phase ->
      [(phase-1)..(phase*4)..(signal.Length-1)]
      |> Seq.map ( fun s1 ->
         let s2 = (s1 + phase) |> min (signal.Length)
         let s3 = (s1 + (2 * phase)) |> min (signal.Length)
         let s4 = (s1 + (3 * phase)) |> min (signal.Length)
      
         let positiveSum = sums.[s2] - sums.[s1] 
         let negativeSum = sums.[s4] - sums.[s3] 
         positiveSum - negativeSum )
      |> Seq.sum )
   |> Seq.map ( fun v -> abs v % 10 )
   |> Seq.toArray

let part1 () =
   [1..100]
   |> Seq.fold (fun s _ -> fft s ) input
   |> Seq.take 8
   |> Seq.map (sprintf "%d")
   |> fun digits -> String.Join( "", Array.ofSeq digits )

let part2 () =
    let offset = 
        input
        |> Seq.take 7
        |> Seq.fold (fun r v -> r * 10 + v ) 0

    [1..100]
    |> Seq.fold ( fun s i -> fft s ) input100
    |> Seq.skip (int offset)
    |> Seq.take 8
    |> Seq.map (sprintf "%d")
    |> fun digits -> String.Join( "", Array.ofSeq digits )
   