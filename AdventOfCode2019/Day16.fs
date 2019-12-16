module Day16

open System
open System.IO

let input =
   File.ReadAllText "Input/day16.txt"
   |> Seq.choose (fun c ->
      match Int32.TryParse (sprintf "%c" c) with
      | true, n -> Some n
      | _ -> None )
   |> Seq.toList

let input100 =
   Seq.replicate 100 input
   |> Seq.concat
   |> Seq.toList

let rec phase n = seq {
   for v in [0;1;0;-1] do
      yield! Seq.replicate n v
   yield! phase n }

let fft (signal:int list) =
   [1..signal.Length]
   |> Seq.map( fun outputIndex ->
      Seq.zip
         signal
         (phase outputIndex |> Seq.skip 1)
      |> Seq.map (fun (v,p) -> v * p )
      |> Seq.sum )
   |> Seq.map ( fun v -> abs v % 10 )
   |> Seq.toList

let part1 () =
   [1..100]
   |> Seq.fold ( fun s _ -> fft s ) input
   |> Seq.take 8
   |> Seq.map (sprintf "%d")
   |> fun digits -> String.Join( "", Array.ofSeq digits )
