module Day4

let private MIN = 172930
let private MAX = 683082

let private quadwise s =
   s 
   |> Seq.pairwise |> Seq.pairwise |> Seq.pairwise
   |> Seq.map( fun (((a,_),(_,_)),((b,c),(_,d))) -> a,b,c,d )

let private isIncreasing n =
   sprintf "%d" n
   |> Seq.pairwise 
   |> Seq.tryFind( fun (a,b) -> a > b )
   |> Option.isNone

let private hasDouble n =
   sprintf "%d" n
   |> Seq.pairwise 
   |> Seq.tryFind( fun (a,b) -> a = b )
   |> Option.isSome

let private hasDoubleStrict n =
   sprintf "_%d_" n
   |> quadwise
   |> Seq.tryFind( fun (a,b,c,d) -> a <> b && b = c && c <> d )
   |> Option.isSome

let private isValid n =
   isIncreasing n && hasDouble n

let private isValidStrict n = 
   isIncreasing n && hasDoubleStrict n
   
let private countPasswords check =
   [MIN..MAX]
   |> Seq.filter( check )
   |> Seq.length

let part1 () =
   countPasswords isValid

let part2 () =
   countPasswords isValidStrict