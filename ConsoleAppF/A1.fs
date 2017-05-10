// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module A1

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Linq
open System.Threading
open System.Threading.Tasks
open Akka.Actor
open Akka.Configuration
open Akka.FSharp


let mutable VERBOSE = false
//let PrintfnWithFlag (twf: Printf.TextWriterFormat<_>) = 
//    if VERBOSE then printfn twf
//    else ()

let VerboseOutput (printFunc: Object) = 
    if VERBOSE then Console.WriteLine(printFunc)
    else ()

let PROCS = Environment.ProcessorCount
let PrintProcs () =
    VerboseOutput (sprintf "... Environment.ProcessorCount: %d" PROCS)

let tid () = Thread.CurrentThread.ManagedThreadId

let config = ConfigurationFactory.ParseString <|  @"
    akka {
        suppress-json-serializer-warning = on
    }
    "

let system = ActorSystem.Create ("FSharp-Akka", config)

let duration f = 
    GC.Collect()
    let timer = Stopwatch.StartNew()
    let res = f ()
    timer.Stop()
    Console.WriteLine (sprintf "$$$ duration: %i ms" timer.ElapsedMilliseconds)
    res

let checksum (M: int[]) =
    let s = M.AsParallel().Sum()
    VerboseOutput (sprintf "... checksum: %d" s)

let maxsum (M: int[]) funcName = 
    let max = M.AsParallel().Max()
    Console.WriteLine(sprintf "$$$ %d max_%s" max funcName)

let refresh (R: int[][]) =
    for row in R do Array.Clear (row, 0, (Array.length row))

let RefreshRow (R: int[]) = 
    Array.Clear (R, 0, R.Length)

let inline print_row_range (R: int[][]) i1 low high =
    let rowstr = [| for j1 in low .. high - 1 -> sprintf "%d" R.[i1].[j1] |]
    VerboseOutput (sprintf "%d %d %s" i1 (low - 1) (String.Join(" ", rowstr)))

let inline PrintRowInRange i1 mlow (R: int[]) rlow rhigh1 =
    let rowstr = [| for j2 in rlow .. rhigh1 - 1 -> sprintf "%d" R.[j2] |]
    VerboseOutput (sprintf "%d %d %s" i1 mlow (String.Join(" ", rowstr)))

let inline FindMaxInNextRow (R: int[]) (R': int[]) (M: int[][]) i1 rlow rhigh1 = 
    for j2 in rlow .. rhigh1 - 1 do
        let x = max R.[j2 - 1] (max R.[j2] R.[j2 + 1])
        // printfn "x = %d, R[j2] = %d" x R'.[j2]
        R'.[j2] <- M.[i1 + 1].[j2 - 1] + x 

let inline FindMaxInNextRowParNaive (R: int[]) (R': int[]) (M: int[][]) i1 rlow rhigh1 = 
    Parallel.For(
        rlow, rhigh1, fun j2 ->
            let x = max R.[j2 - 1] (max R.[j2] R.[j2 + 1])
            // printfn "x = %d, R[j2] = %d" x R'.[j2]
            R'.[j2] <- M.[i1 + 1].[j2 - 1] + x 
    )

(*let inline ActorFindMaxInNextRow (R: int[]) (R': int[]) (M: int[][]) i1 (actors: MailboxProcessor<int*int>[]) j2' rlow rhigh1 = 
    for j2 in rlow .. rhigh1 - 1 do
        let x = max R.[j2 - 1] (max R.[j2] R.[j2 + 1])
        // printfn "x = %d, R[j2] = %d" x R'.[j2]
        R'.[j2] <- M.[i1 + 1].[j2 - 1] + x
        if j2' >0 && j2 = 1 then actors.[j2' - 1].Post (1, R'.[j2])
        if j2 = rhigh1 - 1 then actors.[j2' + 1].Post (0, R'.[j2]) *)


let sequential (M: int[][]) = 
    let N1 = M.Length
    let N2 = M.[0].Length
    let R = Array.zeroCreate<int>(N2 + 2)  // sentinel in 0 and N2+1
    let R' = Array.zeroCreate<int>(N2 + 2)
    Array.blit M.[0] 0 R 1 N2                  //R.[0] and R.[N2+1] are sentinel and initial R with the 2nd row
    // printfn "N1 = %d, N2 = %d" N1 N2
    
    let rec FindMax R R' i1 =
        let (low, high1) = (1, N2 + 1)
        if VERBOSE then PrintRowInRange i1 (low - 1) R low high1   //verbose print
        //printfn "i1 = %d" i1

        if i1 = N1 - 1 then R
        else            
            //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
            FindMaxInNextRow R R' M i1 low high1       
            FindMax R' R (i1 + 1)
    
    FindMax R R' 0


let par_naive (M: int[][]) = 
    let N1 = M.Length
    let N2 = M.[0].Length
    let R = Array.zeroCreate<int>(N2 + 2)  // sentinel in 0 and N2+1
    let R' = Array.zeroCreate<int>(N2 + 2)
    Array.blit M.[0] 0 R 1 N2                  //R.[0] and R.[N2+1] are sentinel and initial R with the 2nd row

    let rec FindMax R R' i1 =
        let (low, high1) = (1, N2 + 1)
        if VERBOSE then PrintRowInRange i1 (low - 1) R low high1   //verbose print
        //printfn "i1 = %d" i1

        if i1 = N1 - 1 then R
        else            
            //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
            FindMaxInNextRowParNaive R R' M i1 low high1 |> ignore      
            FindMax R' R (i1 + 1)
    
    FindMax R R' 0        


let par_range (W:(int*int)[]) (M: int[][]) = 
    let N1 = M.Length
    let N2 = M.[0].Length
    let R = Array.zeroCreate<int>(N2 + 2)  // sentinel in 0 and N2+1
    let R' = Array.zeroCreate<int>(N2 + 2)
    Array.blit M.[0] 0 R 1 N2                  //R.[0] and R.[N2+1] are sentinel and initial R with the 2nd row
    // let K = W.Length

    let rec FindMax R R' i1 =
        if i1 = N1 - 1 then R
        else
            Parallel.ForEach(W, fun (low, high1) ->
                if VERBOSE then 
                    PrintRowInRange i1 low R (low + 1) (high1 + 1)   //verbose print
                //printfn "i1 = %d" i1
                //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
                FindMaxInNextRow R R' M i1 (low + 1) (high1 + 1)
                if VERBOSE && (i1 = N2 - 2) then
                    PrintRowInRange i1 low R' (low + 1) (high1 + 1)
            ) |> ignore

            FindMax R' R (i1 + 1)
    
    FindMax R R' 0 


let async_range (W:(int*int)[]) (M: int[][]) = 
    let N1 = M.Length
    let N2 = M.[0].Length
    let R = Array.zeroCreate<int>(N2 + 2)  // sentinel in 0 and N2+1
    let R' = Array.zeroCreate<int>(N2 + 2)
    Array.blit M.[0] 0 R 1 N2                  //R.[0] and R.[N2+1] are sentinel and initial R with the 2nd row
    let K = W.Length

    let rec FindMax R R' i1 =
        if i1 = N1 - 1 then R
        else
            let asyncs = 
                [| for j2 in 0 .. K - 1 ->
                    async {
                        let (low, high1) = W.[j2]
                        if VERBOSE then 
                            PrintRowInRange i1 low R (low + 1) (high1 + 1)   //verbose print
                        //printfn "i1 = %d" i1
                        //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
                        FindMaxInNextRow R R' M i1 (low + 1) (high1 + 1)
                        if VERBOSE && (i1 = N2 - 2) then
                            PrintRowInRange i1 low R' (low + 1) (high1 + 1)
                    }
                |]
            asyncs |> Async.Parallel |> Async.RunSynchronously |> ignore

            FindMax R' R (i1 + 1)
    
    FindMax R R' 0 


(*
type fifo = list<int> * list<int>

let put (x:int) (f:fifo) = 
    let f1, f2 = f
    (x::f1, f2)

let rec get (f:fifo) =
    match f with
    | ([], []) -> failwith "empty"
    | (f1, y::f2) -> y, (f1, f2)
    | (f1, []) -> get ([], List.rev (f1))

let isempty (f:fifo) =
    f = ([], [])
*)

let mailbox_range (W:(int*int)[]) (M: int[][]) = 
    let N1 = M.Length
    let N2 = M.[0].Length
    let K = W.Length

    let resultR = Array.zeroCreate<int>(N2 + 2)   // sentinel in 0 and N2+1
    let actors_started = TaskCompletionSource<bool>()
    let actors_complete = TaskCompletionSource<bool>()

    // actors will post and receive a pair of integer, the first int is 0 or 1, 
    // 0 means it is posted from left actor and 1 means the right actor
    // The second int is the real value need to compute
    let actors = Array.zeroCreate<MailboxProcessor<int*int>>(K)
    let count = ref K

    for j2 in 0 .. K - 1 do
        actors.[j2] <- MailboxProcessor.Start(fun inbox ->
            if Interlocked.Decrement(count) = 0 then
                count := K
                actors_started.SetResult true

            // actors run asynchronously, only coordinated by messages
            // thus each actor needs its own pair of rows

            let (low, high1) = W.[j2]
            let N2' = high1 - low       //columm number controlled by each actor
            let (low', high1') = (0, N2')
            
            let R = Array.zeroCreate<int>(N2' + 2)    // sentinel in 0 and N2'+1
            let R' = Array.zeroCreate<int>(N2' + 2)
            Array.blit M.[0] low R 1 N2'
        
            let rec FindMax R R' i1 =
                async {
                    if i1 = N1 - 1 then 
                        Array.blit R 1 resultR (low + 1) N2'
                        if Interlocked.Decrement(count) = 0 then
                            actors_complete.SetResult true
                        ()
                    else
                        try
                            // rightmost (j2=K-1) actor's receive from right side could be avoided
                            // firstly get the left and right item from the inbox but need to judge
                            let! m = inbox.Receive()
                            let! n = inbox.Receive()
                            
                            if fst m = 0 then R.[low'] <- snd m
                            else R.[high1' + 1] <- snd m 
                            
                            if fst n = 1 then R.[high1' + 1] <- snd n  // +1!
                            else R.[low'] <- snd n

                            VerboseOutput (sprintf "... actor %2d row %2d received %3d and %3d" j2 i1 R.[low'] R.[high1' + 1])

                            if VERBOSE then 
                                PrintRowInRange i1 low R (low' + 1) (high1' + 1)   //verbose print
                            //printfn "i1 = %d" i1
                            //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
                            FindMaxInNextRow R R' M i1 (low' + 1) (high1' + 1)
                            if VERBOSE && (i1 = N2 - 2) then
                                PrintRowInRange i1 low R' (low + 1) (high1 + 1)

                            if j2 > 0 then actors.[j2 - 1].Post (1, R'.[low' + 1])  // +1!
                            if j2 < K - 1 then actors.[j2 + 1].Post (0, R'.[high1'])

                            VerboseOutput (sprintf "... actor %2d row %2d posted %3d and %3d" j2 i1 R'.[low' + 1] R'.[high1'])

                            return! FindMax R' R (i1 + 1)
                        with
                            | ex -> Console.WriteLine( sprintf "*** row %d actor %d exception: %s" i1 j2 ex.Message )
                }

            FindMax R R' 0                            
        )              

    actors_started.Task.Wait() // not really needed here

    //Console.WriteLine( sprintf "... posting rightmost column completed" )    
    for i1 in 1..N1-1 do
        actors.[0].Post (0, 0)    // "sentinel posts" left

    //Console.WriteLine( sprintf "... posting first row completed" )    
    for j2 in 0 .. K - 1 do
        if j2 > 0 then actors.[j2].Post (0, M.[0].[fst W.[j2] - 1])
        if j2 < K - 1 then actors.[j2].Post (1, M.[0].[fst W.[j2]])
    
    //Console.WriteLine( sprintf "... posting rightmost column completed" )
    for i1 in 1..N1-1 do
        actors.[K-1].Post (1, 0)  // "sentinel posts" right

    actors_complete.Task.Wait()
    resultR


let Akka_range (W:(int*int)[]) (M: int[][]) = 
    let N1 = M.Length
    let N2 = M.[0].Length
    let K = W.Length

    let resultR = Array.zeroCreate<int>(N2 + 2)   // sentinel in 0 and N2+1
    let actors_started = TaskCompletionSource<bool>()
    let actors_complete = TaskCompletionSource<bool>()

    // actors will post and receive a pair of integer, the first int is 0 or 1, 
    // 0 means it is posted from left actor and 1 means the right actor
    // The second int is the real value need to compute
    let actors = Array.zeroCreate<IActorRef>(K)
    let count = ref K

    for j2 in 0 .. K - 1 do
        actors.[j2] <- spawn system (sprintf "actor_%d" j2) 
            <| fun (inbox: Actor<int*int>) ->
                    if Interlocked.Decrement(count) = 0 then
                        count := K
                        actors_started.SetResult true

                    // actors run asynchronously, only coordinated by messages
                    // thus each actor needs its own pair of rows

                    let (low, high1) = W.[j2]
                    let N2' = high1 - low       //columm number controlled by each actor
                    let (low', high1') = (0, N2')
            
                    let R = Array.zeroCreate<int>(N2' + 2)    // sentinel in 0 and N2'+1
                    let R' = Array.zeroCreate<int>(N2' + 2)
                    Array.blit M.[0] low R 1 N2'
        
                    let rec FindMax R R' i1 =
                        actor {
                            if i1 = N1 - 1 then 
                                Array.blit R 1 resultR (low + 1) N2'
                                if Interlocked.Decrement(count) = 0 then
                                    actors_complete.SetResult true
                                ()
                            else
                                try
                                    // rightmost (j2=K-1) actor's receive from right side could be avoided
                                    // firstly get the left and right item from the inbox but need to judge
                                    let! m = inbox.Receive()
                                    let! n = inbox.Receive()
                                    
                                    if fst m = 0 then R.[low'] <- snd m
                                    else R.[high1' + 1] <- snd m 
                                    
                                    if fst n = 1 then R.[high1' + 1] <- snd n
                                    else R.[low'] <- snd n

                                    VerboseOutput (sprintf "... actor %2d row %2d received %3d and %3d" j2 i1 R.[low'] R.[high1' + 1])

                                    if VERBOSE then 
                                        PrintRowInRange i1 low R (low' + 1) (high1' + 1)   //verbose print
                                    //printfn "i1 = %d" i1
                                    //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
                                    FindMaxInNextRow R R' M i1 (low' + 1) (high1' + 1)
                                    if VERBOSE && (i1 = N2 - 2) then
                                        PrintRowInRange i1 low R' (low + 1) (high1 + 1)

                                    if j2 > 0 then actors.[j2 - 1] <! (1, R'.[low' + 1])
                                    if j2 < K - 1 then actors.[j2 + 1] <! (0, R'.[high1'])

                                    VerboseOutput (sprintf "... actor %2d row %2d posted %3d and %3d" j2 i1 R'.[low' + 1] R'.[high1'])

                                    return! FindMax R' R (i1 + 1)
                                with
                                    | ex -> Console.WriteLine( sprintf "*** row %d actor %d exception: %s" i1 j2 ex.Message )
                        }

                    FindMax R R' 0                            
                      

    actors_started.Task.Wait() // not really needed here

    //Console.WriteLine( sprintf "... posting rightmost column completed" )    
    for i1 in 1..N1-1 do
        actors.[0] <! (0, 0)    // "sentinel posts" left

    //Console.WriteLine( sprintf "... posting first row completed" )    
    for j2 in 0 .. K - 1 do
        if j2 > 0 then actors.[j2] <! (0, M.[0].[fst W.[j2] - 1])
        if j2 < K - 1 then actors.[j2] <! (1, M.[0].[fst W.[j2]])
    
    //Console.WriteLine( sprintf "... posting rightmost column completed" )
    for i1 in 1..N1-1 do
        actors.[K-1] <! (1, 0)  // "sentinel posts" right

    actors_complete.Task.Wait()
    resultR


[<EntryPoint>]
let main argv = 
    // fname /alg k v

    try 
        PrintProcs()

        let (fname, alg, k, v) = (argv.[0], argv.[1], argv.[2], argv.[3])       

        if Int32.Parse(v) = 1 then VERBOSE <- true
        VerboseOutput (sprintf "... command line args: %A" argv)

        let fname = argv.[0]
        let M = Data.getData fname
        VerboseOutput (sprintf "... input: %A" M)

        let GetPartitions = 
            let partitions = Int32.Parse k
            let N2 = M.[0].Length
            let parSize = fun () ->
                match partitions with 
                | 0 -> 0
                | _ -> if partitions > N2 then 1 else N2 / partitions

            match partitions with 
            | 0 -> Partitioner.Create(0, N2).AsParallel().ToArray() |> Array.sort
            | _ -> seq { for j2 in 0 .. (partitions - 1) ->
                            if j2 <> (partitions - 1) then
                                (j2 * parSize(), (j2 + 1) * parSize())
                            else 
                                (j2 * parSize(), N2) } |> Array.ofSeq |> Array.sort
        
        let run funcName func =
            Console.WriteLine (sprintf "\r\n$$$ %s" funcName)
            let R = duration (fun () -> func M)
            maxsum R funcName

        match alg with
        | "/SEQ" -> run "sequential (no range)" sequential
        | "/PAR-NAÏVE" | "/PAR-NAIVE" -> run "par_naïve (no range)" par_naive
        | "/PAR-RANGE" -> run "par_range" (par_range GetPartitions)
        | "/ASYNC-RANGE" -> run "async_range" (async_range GetPartitions)
        | "/MAILBOX-RANGE" -> run "mailbox_range" (mailbox_range GetPartitions)
        | "/AKKA-RANGE" -> run "Akka_range" (Akka_range GetPartitions)
        | "*" -> 
            run "sequential (no range)" sequential
            run "par_naïve (no range)" par_naive
            run "par_range" (par_range GetPartitions)
            run "async_range" (async_range GetPartitions)
            run "mailbox_range" (mailbox_range GetPartitions)
            run "Akka_range" (Akka_range GetPartitions)
        | _ -> ()

    
        0 // return an integer exit code
    with
    | ex -> 
        Console.Error.WriteLine (sprintf "*** %s" ex.Message)
        1

