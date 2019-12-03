module Day3

open System
open System.IO

type Segment = { X : int; Y : int; SegmentLen : int; WireLen : int }

let private parseWire (line:string) =
   line.Split ','
   |> Array.map (fun s -> s.[0], Int32.Parse( s.[1..] ) )
   |> Array.fold( fun (x,y,total,vert,horiz) (dir,len) ->
      match dir with
      | 'L' -> 
         let h = horiz |> Set.add { X = x; Y = y; SegmentLen = -len; WireLen = total }
         ( x-len, y, total+len, vert, h )
      | 'R' -> 
         let h = horiz |> Set.add { X = x; Y = y; SegmentLen = len; WireLen = total }
         ( x+len, y, total+len, vert, h )
      | 'U' -> 
         let v = vert |> Set.add { X = x; Y = y; SegmentLen = len; WireLen = total }
         ( x, y+len, total+len, v, horiz )
      | 'D' -> 
         let v = vert |> Set.add { X = x; Y = y; SegmentLen = -len; WireLen = total }
         ( x, y-len, total+len, v, horiz)
      | _ -> 
         ( x, y, total+len, vert, horiz ) )
      (0,0,0,Set.empty,Set.empty)
   |> fun (_,_,_,vert,horiz) -> vert,horiz

let private input =
   File.ReadLines "Input/day3.txt"
   |> Seq.map parseWire
   |> Seq.toArray

let private isBetween a b x =
   let l,r = if a < b then a,b else b,a
   l <= x && x <= r

let private intersect (vert:Segment) (horiz:Segment) =
   let xintersect = vert.X |> isBetween horiz.X (horiz.X+horiz.SegmentLen)
   let yintersect = horiz.Y |> isBetween vert.Y (vert.Y+vert.SegmentLen)

   if xintersect && yintersect 
   then Some (vert.X,horiz.Y, (vert.WireLen + abs (horiz.Y - vert.Y)) + (horiz.WireLen + abs (vert.X - horiz.X)))
   else None

let private intersections =
   let vert0,horiz0 = input.[0]
   let vert1,horiz1 = input.[1]

   Seq.append
      (Seq.allPairs vert0 horiz1)
      (Seq.allPairs vert1 horiz0)
   |> Seq.choose( fun (v,h) -> intersect v h )

let part1 () =
   intersections
   |> Seq.map( fun (x,y,_) -> x + y )
   |> Seq.sort
   |> Seq.head

let part2 () =
   intersections
   |> Seq.map( fun (_,_,d) -> d )
   |> Seq.sort
   |> Seq.head
