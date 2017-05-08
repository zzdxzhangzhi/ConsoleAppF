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

task.Start()
task.Wait(1000)
let r1 = task.Result

let t1 = Task.Factory.StartNew(fun () -> 
                                    printfn "new task begin.."
                                    10)
let t2  = Task.Factory.StartNew(fun o ->
                                    printfn "new task start with %A .." o, obj)

let t3 = Task.Run(fun () -> 
                        printfn "new task begin.."
                        10)

let cont = task.ContinueWith(fun (t: Task<int>) -> 
                                 printfn "continue task with %A " t
                                 t.Result)

let cont1 = Task.Factory.ContinueWhenAny([|t1; task; t3|], 
                                         fun (t: Task<int>) -> 
                                                printfn "continue task for any ... " 
                                                t.Result)

let cont2 = Task.Factory.ContinueWhenAll([|t1; task; t3|], 
                                         fun (t: Task<int>[]) -> 
                                                let mutable r = 0
                                                for x in t do 
                                                    printfn "continue task for all ... " 
                                                    r <- r + x.Result
                                                r)

let rec SeqSumTree (tr: Tree<int>) =
    match tr with
    | Leaf data -> data
    | Tree (data, lchild, rchild) ->
        data + (SeqSumTree lchild) + (SeqSumTree rchild)

let rec CreateTree (level: int) (data: int) : Tree<int> = 
    match level with 
    |0 -> Leaf data
    |_ -> Tree (data, 
                CreateTree (level - 1) data, 
                CreateTree (level - 1) data)

let taskCount = ref 0
let threadDictionary = new ConcurrentDictionary<int, int>()

let rec ParSumTree (tr: Tree<int>) (depth: int) (threshold: int) =
    match tr with 
    |Leaf data -> data
    |Tree (data, lchild, rchild) ->
        if depth >= threshold then
            // Recorde the tread ID and increment the record of it
            threadDictionary.AddOrUpdate(
                Thread.CurrentThread.ManagedThreadId,
                1,
                fun k v -> v + 1) |> ignore

            let sum = SeqSumTree tr
            sum
        else
            //Fork one or more task for left branch
            let leftTask = Task.Factory.StartNew(fun () ->
                ParSumTree lchild (depth + 1) threshold)
            
            //record the task count number
            Interlocked.Increment(taskCount) |> ignore

            //continue same task and get the sum of the right branch
            let rightSum = ParSumTree rchild (depth + 1) threshold

            //after completing the right branch, then join the task for the left branch
            let sum = data + rightSum + leftTask.Result
            sum

let SequentialSum (tree: Tree<int>) =
    printfn "\r\n Sequential Sum begin ..."
    let sw = Stopwatch.StartNew()
    let sum = SeqSumTree tree
    sw.Stop()
    printfn "... Sequential result: ... %.3fs ... %d" 
        (float sw.ElapsedMilliseconds / 1000.0) sum

let ParallelSum (tree: Tree<int>) (threshold: int) =
    printfn "\r\n Parallel Sum begin ..."
    printfn "... Sequential Theshold: %d" threshold

    taskCount := 0
    let sw = Stopwatch.StartNew()
    let sum = ParSumTree tree 0 threshold
    sw.Stop()
    printfn "... Parallel result: ... %.3fs ... %d" 
        (float sw.ElapsedMilliseconds / 1000.0) sum
    printfn "... Task Count: %d" (1 + !taskCount)
    printfn "... Tread used (%d): %A" threadDictionary.Count (threadDictionary.ToArray())

//[<EntryPoint>]
let Main args =
    printfn "Environment Processor Count: %d" Environment.ProcessorCount

    let tr2 = CreateTree 2 1
    printfn "tr2: %A" tr2
    printfn "SeqSumTree tr2 = %d" (SeqSumTree tr2)

    taskCount := 0
    printfn "ParSumTree tr2 = %d [Task Count = %d]" (ParSumTree tr2 0 3) !taskCount
    
    let depth = 24

    printfn "... Creating a tree of level %d, with %d intermediate nodes"
        depth (int (Math.Pow(2.0, float depth)) - 1)
    
    let atree = CreateTree depth 1

    SequentialSum atree

    for threshold in [0; 1; 2; 4; 8; 10; 12; 16; 17; 18; 19; 20] do
        ParallelSum atree threshold
    
    0

Main() |> ignore

let psum = ref 0
psum := 0

let A = Array.init 1000000 (fun i -> 1)

Parallel.For(0, A.Length,
    fun () -> 
        let threadsum = ref 0
        printfn "beginning thread own sum"
        threadsum
    , fun i loopstate threadsum ->
        threadsum := !threadsum + A.[i]
        threadsum
    , fun threadsum ->
        printfn "Interactive with the whole sum"
        Interlocked.Add(psum, !threadsum) |> ignore
        ()
    ) |> ignore

printfn "Parallel for sum = %d" !psum

open Microsoft.FSharp.Collections
open System.Linq

for (low, high1) in
    (Partitioner.Create(0, 31, 10))
        .AsParallel().AsEnumerable() do
    printfn "%d %d %d" (high1 - low) low high1

let ParCompute P MAX =    
    let A = Array.init 1000000 (fun i -> 1)
    let AP = Array.create (A.Length * P) 0
    for k = 0 to (A.Length - 1) do AP.[k * P] <- A.[k]

    let sw = Stopwatch.StartNew()
    for k = 0 to (A.Length - 1) do
        for i = 1 to MAX do
            A.[k] <- A.[k] + 1
    sw.Stop()
    printfn "... Sequential result: ... %.3fs" 
        (float sw.ElapsedMilliseconds / 1000.0)

    sw.Restart()
    Parallel.For (0, A.Length, (fun k ->
        for i = 1 to MAX do
            AP.[k * P] <- AP.[k * P] + 1)) |> ignore
    
    for k = 0 to (A.Length - 1) do A.[k] <- AP.[k * P]
    sw.Stop()
    printfn "... Parallel result: ... %.3fs" 
        (float sw.ElapsedMilliseconds / 1000.0)

ParCompute 4 1000

let ParSeqContrast K N size = 
    let PROCS = Environment.ProcessorCount;
    let A = Array.init size (fun i -> 1)
    
    let sw = Stopwatch.StartNew()
    for k = 1 to K do
        for i = 0 to size - 1 do
            for n = 1 to N do
                A.[i] <- A.[i] + 1
    sw.Stop()
    printfn "... Sequential result: ... %.3fs" 
        (float sw.ElapsedMilliseconds / 1000.0)

    sw.Restart()
    for k = 1 to K do
        Parallel.For (0, size, fun i ->
            for n = 1 to N do
                A.[i] <- A.[i] + 1) |> ignore    
    
    sw.Stop()
    printfn "... Parallel result: ... %.3fs" 
        (float sw.ElapsedMilliseconds / 1000.0)

    sw.Restart()
    for k = 1 to K do
        Parallel.ForEach (
            Partitioner.Create(0, size, size/(3 * PROCS)),
            fun (low, high1) ->
                //printf "... %d" (high1 - low)
                for i = low to high1 - 1 do
                    for n = 1 to N do
                        A.[i] <- A.[i] + 1) |> ignore    
    
    sw.Stop()
    printfn "\r\n... Partion Parallel result: ... %.3fs" 
        (float sw.ElapsedMilliseconds / 1000.0)
    
    sw.Restart()
    for k = 1 to K do
        Partitioner.Create(0, size, size/(3 * PROCS))
            .AsParallel().ForAll(
                fun (low, high1) ->
                    //printf "... %d" (high1 - low)
                    for i = low to high1 - 1 do
                        for n = 1 to N do
                            A.[i] <- A.[i] + 1) |> ignore    
    
    sw.Stop()
    printfn "\r\n... PLINQ Partion Parallel result: ... %.3fs" 
        (float sw.ElapsedMilliseconds / 1000.0)

ParSeqContrast 100 4 1000000
            
let async0 = async {return 10 + 10}
let res = Async.RunSynchronously async0
printfn "async0: %A" async0
printfn "async0 result: %A" res

Async.StartWithContinuations(
    async0,
    (fun n -> printfn "success %d" n),
    (fun x -> printfn "exception %s" x.Message),
    (fun _ -> printfn "cancellation"))

let task0 = Async.StartAsTask async0
printfn "do some other work ..."
let r2 = task0.Result
printfn "done r2: %d" r2

let task1 = async {return 10 + 10}
let task2 = async {
                do! Async.Sleep(2000)
                return 20 + 20
            }

let res1 = Async.RunSynchronously(Async.Parallel [task1; task2])
let res2 = [task1; task2] |> Async.Parallel |> Async.RunSynchronously
printfn "2 tasks result: %A %A" res1 res2

let tid () = Thread.CurrentThread.ManagedThreadId    
let simulatedJob id time = 
    async {
        printfn "... [%d] Job %d starts" (tid()) id
        do! Async.Sleep(1000 * time)
        printfn "... [%d] Job %d ends" (tid()) id
        return id
    }

Async.RunSynchronously(simulatedJob 11 3)
Async.StartWithContinuations(simulatedJob 12 3,
    (fun r -> printfn "success %d" r),
    (fun x -> printfn "exception %s" x.Message),
    (fun _ -> printfn "cancellation"))

let task' = Async.StartAsTask (simulatedJob 13 4)
printfn "do some other work ..."
let r3 = task'.Result
printfn "done r3: %d" r3

let r4 = 
    [2; 4; 6]
    |> List.mapi (fun index time -> 
                    simulatedJob index time)
    |> Async.Parallel
    |> Async.RunSynchronously

let tid1 () = Thread.CurrentThread.ManagedThreadId
let fun3 = 
    async {
        printfn "... [%d] fun3 starts" (tid1())
        do! Async.Sleep(1000)
        printfn "... [%d] fun3 ends" (tid1())
    }

let fun2 = 
    async {
        printfn "... [%d] fun2 starts" (tid1())
        do! fun3
        printfn "... [%d] fun2 middle ends" (tid1())
        do! fun3
        printfn "... [%d] fun2 ends" (tid1())
    }

let fun1 = 
    async {
        printfn "... [%d] fun1 starts" (tid1())
        do! fun2
        printfn "... [%d] fun1 ends" (tid1())
    }

do Async.RunSynchronously fun1

let incr x = 
    x := !x + 1
let agent1 = MailboxProcessor.Start(fun inbox ->
    let count = ref 0
    async {
        while true do
            let! msg = inbox.Receive()
            incr count |> ignore
            printfn "... total %d messages" !count
    })

["the"; "quick"; "brown"; "fox"]
|> List.map agent1.Post

let agent2 = MailboxProcessor.Start(fun inbox ->
    let rec loop count = 
        async {
            let! msg = inbox.Receive()
            printfn "... agent 2 total %d messages" (count + 1)
            return! loop (count + 1)
        }
    loop 0
    )

["Jumps"; "over"; "the"; "lazy"; "dog"]
|> List.map agent2.Post

type Message = 
    |Toggle
    |Add of int
    |Get of AsyncReplyChannel<int>

let agent = MailboxProcessor<Message>.Start(fun inbox ->
    let rec active n =
        async {
            printfn "... active %d" n
            let! msg = inbox.Receive()
            match msg with 
            |Toggle -> return! inactive n
            |Add m -> return! active (n + m)
            |Get ch -> ch.Reply n; return! active n
        }
    and inactive n = 
        async {
            printfn "... inactive %d" n
            let! msg = inbox.Receive()
            match msg with
            |Toggle -> return! active n
            |Add m -> return! inactive n
            |Get ch -> ch.Reply n; return! inactive n
        }
    
    active 0
    )
        
agent.Post (Add 10)
agent.Post Toggle
agent.Post (Add 20)
agent.Post Toggle
agent.Post (Add 30)

let n1 = agent.PostAndReply(fun ch -> (Get ch))
let n2 = agent.PostAndReply Get
let n3 = agent.PostAndAsyncReply(fun ch -> (Get ch))
let n3' = Async.RunSynchronously n3

printfn "... got %d %d %d" n1 n2 n3'

let asyncs = 
    [ for i in 0 .. 100000 -> 
        async {
            Async.Sleep(1000) |> ignore
            if i % 10000 = 0 then
                printfn "async %d" i
        }
    ]
asyncs |> Async.Parallel |> Async.RunSynchronously

let agents = 
    [ for i in 0 .. 100000 ->
        MailboxProcessor.Start(fun inbox ->
            async {
                while true do
                    let! msg = inbox.Receive()
                    if i % 10000 = 0 then
                        printfn "agent %d got message '%s'" i msg
            }
        )
    ]

for agent in agents do
    agent.Post "Ping!"

let incr1 x = 
    x := !x + 1
let rec und = MailboxProcessor.Start(fun inbox ->
    let count = ref 0
    async {
        while true do
            und.Post 1
            let! msg = inbox.Receive()
            match msg with
            |1 -> incr1 count
            |_ -> printfn "... %d" !count
    })

for i = 0 to 10000000 do
    if i = 10000000 then und.Post 0
    else 
        und.Post 1
        if i % 1000000 = 0 then 
            printfn "Post %d for 1" i



