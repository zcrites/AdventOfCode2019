module Day16

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

let fft signal =
   let sums =
      signal |> Array.scan (+) 0

   [1..signal.Length]
   |> Seq.map( fun phase ->
      [(phase-1)..(phase*4)..(signal.Length-1)]
      |> Seq.map ( fun s1 ->
         let s2 = s1 + phase |> min signal.Length
         let s3 = s1 + 2 * phase |> min signal.Length
         let s4 = s1 + 3 * phase |> min signal.Length
      
         let positiveSum = sums.[s2] - sums.[s1] 
         let negativeSum = sums.[s4] - sums.[s3] 
         positiveSum - negativeSum )
      |> Seq.sum )
   |> Seq.map ( fun v -> abs v % 10 )
   |> Seq.toArray

let printSignal signal =
    signal
    |> Seq.fold ( fun r v -> r * 10 + v ) 0
    |> sprintf "%d"

let part1 () =
   [1..100]
   |> Seq.fold ( fun signal _ -> fft signal ) input
   |> Seq.take 8
   |> printSignal
   
let fftBackside reversedSignal =
   reversedSignal
   |> Seq.scan (+) 0
   |> Seq.skip 1
   |> Seq.map ( fun v -> v % 10 )

let part2 () =
   let offset = 
      input
      |> Seq.take 7
      |> Seq.fold ( fun r v -> r * 10 + v ) 0

   [1..100]
   |> Seq.fold ( fun signal _ -> fftBackside signal ) (Seq.rev input100.[offset..])
   |> Seq.rev
   |> Seq.take 8
   |> printSignal