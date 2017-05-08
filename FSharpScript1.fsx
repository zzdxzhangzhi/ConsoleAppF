let fibseq n = 
    let rec f m r1 r2 =
        seq {
            match m with
            |0 -> yield r2
            |_ -> 
                yield r2 
                yield! f (m - 1) r2 (r1 + r2)
        }
    f n 0. 1.
    
for num in (fibseq 35) do printf "%.0f " num

let fibseq' n = Seq.unfold(
                    fun (m, r1, r2) -> 
                        if m < 0 then None 
                        else Some(r2, (m - 1, r2, r1 + r2))
                ) (n, 0., 1.)
                
for num in (fibseq' 35) do printf "%.0f " num

type Tree<'T> =
    |Leaf of 'T
    |Tree of 'T * Tree<'T> * Tree<'T>

let tree = Tree(10, Tree(20, Leaf 30, Leaf 40), Leaf 50)
let tree' = Leaf 100
let tree'' = Tree(10, Leaf 20, Leaf 30)

printfn "tree, tree', tree'': %A, %A, %A" tree tree' tree''

let rec size t = 
    match t with
    |Leaf _ -> 1
    |Tree(_, l, r) -> 1 + (size l) + (size r)
    
printfn "size of tree: %d" (size tree)

let rec sum t =
    match t with
    |Leaf x -> x
    |Tree(x, l, r) -> x + (sum l) + (sum r)
    
printfn "sum of tree: %d" (sum tree)

let printt t = 
    let rec printtr indent t = 
        match t with 
        |Leaf x -> printfn "%s%d" indent x
        |Tree (x, l, r) -> 
            printfn "%s%d" indent x
            printtr (indent + "     ") l
            printtr (indent + "     ") r
    printtr "" t
    
printt tree

let printtin t = 
    let rec printtr indent t = 
        match t with 
        |Leaf x -> printfn "%s%d" indent x
        |Tree (x, l, r) ->             
            printtr (indent + "     ") l
            printfn "%s%d" indent x
            printtr (indent + "     ") r
    printtr "" t
    
printtin tree

let e = Seq.singleton 10
let f = Seq.length [10; 20; 30]
let g = Seq.map (fun x -> x + 1) [10; 20; 30]
let h = Seq.item 2 [10; 20; 30]
let i = Seq.empty<int>

printfn "e = %A, f = %A, g = %A, h = %A, i = %A" e f g h i

let j = Seq.fold (fun s x -> s + x) 0 [10; 20; 30]
let k = Seq.sum [10; 20; 30]
let l = Seq.concat [[11; 12; 13]; [21; 22; 23]; [31; 32; 33;]]
let m = Seq.collect (fun x -> [x + 1; x + 2; x + 3;]) [10; 20; 30]
printfn "j = %A, k = %A, l = %A, m = %A" j k l m

let g0 = {10 .. 10 .. 30}
let inc1 = fun x -> x + 1

let seqinc1 = Seq.map inc1
let g1 = seqinc1 g0

printfn "g1 = %A" g1

let inc2 x = Seq.singleton (x + 1)
let seqinc1' = Seq.map inc2
let seqinc2 = Seq.collect inc2

let g1' = seqinc1' g0
let g2 = seqinc2 g0

printfn "g1' = %A, g2 = %A" g1' g2

let o = Seq.filter (fun x -> x % 4 <> 0) [10; 20; 30]
let p = Seq.init 3 (fun i -> 100)
let q = Seq.init 3 (fun i -> (i + 1) * 100)
let r = Seq.initInfinite (fun i -> (i + 1) * 100)
let z = Seq.zip [1; 2; 3] [10; 20; 30; 40; 50]

printfn "o = %A, p = %A, \nq = %A, r = %A, \nz = %A"
    o p q r z

let head :: tail = [1; 2; 3]
let head1 = [1; 2; 3].Head
let tail1 = [1; 2; 3].Tail
let head2 = List.head [1; 2; 3]
let tail2 = List.tail [1; 2; 3]

printfn "head = %A, tail = %A; \nhead1 = %A, tail1 = %A; \nhead2 = %A, tail2 = %A"
    head tail head1 tail1 head2 tail2

let k1 = [20; 10]
let m1 = 30 :: k1
let m1' = 31 :: k1

let (h1, n) = List.head m1, List.tail m1
printfn "h1 = %A, n = %A" h1 n

let l0 = [10 .. 10 .. 30]
let linc1 = fun x -> x + 1

let listinc1 = List.map linc1
let l1 = listinc1 l0

printfn "l1 = %A" l1

let linc2 x = [x + 1]
let listinc1' = List.map linc2
let listinc2 = List.collect linc2

let l1' = listinc1' l0
let l2 = listinc2 l0

printfn "l1' = %A, l2 = %A" l1' l2

let a = [|1 .. 3|]
printfn "a = %A" a

let l3 = [|1 .. 3|].[2]
a.[2] <- 10

printfn "a = %A, l3 = %A" a l3

let a0 = [|10 .. 10 .. 30|]
let ainc1 = fun x -> x + 1

let arrayinc1 = Array.map ainc1
let a1 = arrayinc1 a0

printfn "a1 = %A" a1

let ainc2 x = [|x + 1|]
let arrayinc1' = Array.map ainc2
let arrayinc2 = Array.collect ainc2

let a1' = arrayinc1' a0
let a2 = arrayinc2 a0

printfn "a1' = %A, a2 = %A" a1' a2

let a11 = Array.init 10000000 (fun i -> 100)
let b1 = Array.map (fun x -> x + 1) a11

printfn "%A, \n%A" a11 b1 

let a22 = Array.Parallel.init 10000000 (fun i -> 10)
let b2 = a22 |> Array.Parallel.map (fun x -> x + 1)

printfn "%A, \n%A" a22 b2

let fq = query {for x in [10; 20; 30] do count}
let gq = query {for x in [10; 20; 30] do select (x + 1)}
printfn "gq = %A" gq

let hq = query {for x in [10; 20; 30] do sumBy x}
let iq = query {for x in [10; 20; 30] do nth 2}

let jq = query {for x in [[11; 12; 13];
                          [21; 22; 23];
                          [31; 32; 33]] do
                    for y in x do select y}

let kq = query {for x in [10; 20; 30] do 
                for y in [x + 1; x + 2; x + 3] do select y}

let lq = query {for x in [10; 20; 30] do
                where (x % 4 <> 0) 
                select x}

printfn "kq = %A, lq = %A" kq lq

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

let task = new Task<int>(fun () -> 
                            printfn "new task begin.."
                            10)
task.

