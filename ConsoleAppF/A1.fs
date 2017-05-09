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
    let K = W.Length

    let rec FindMax R R' i1 =
        let (low, high1) = W.[j2]
        if VERBOSE then PrintRowInRange i1 (low - 1) R low high1   //verbose print
        //printfn "i1 = %d" i1

        if i1 = N1 - 1 then R
        else            
            //Array.blit M.[i1 + 1] 0 R' 1 N2   //every loop assign original next row to R'           
            FindMaxInNextRowParNaive R R' M i1 low high1 |> ignore      
            FindMax R' R (i1 + 1)
    
    FindMax R R' 0 


let async_range (W:(int*int)[]) (M: int[][]) = 
    M.[0]

let mailbox_range (W:(int*int)[]) (M: int[][]) = 
    M.[0]

let Akka_range (W:(int*int)[]) (M: int[][]) = 
    M.[0]


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
            match partitions with 
            | 0 -> Partitioner.Create(0, N2).AsParallel().ToArray() |> Array.sort
            | _ -> 
        let W = GetPartitions()

        let run funcName func =
            Console.WriteLine (sprintf "\r\n$$$ %s" funcName)
            let R = duration (fun () -> func M)
            maxsum R funcName

        match alg with
        | "/SEQ" -> run "sequential (no range)" sequential
        | "/PAR-NAÏVE" | "/PAR-NAIVE" -> run "par_naïve (no range)" par_naive
        | "/PAR-RANGE" -> run "par_range" (par_range W)
        | "/ASYNC-RANGE" -> run "async_range" (async_range W)
        | "/MAILBOX-RANGE" -> run "mailbox_range" (mailbox_range W)
        | "/AKKA-RANGE" -> run "Akka_range" (Akka_range W)
        | _ -> ()

    
        0 // return an integer exit code
    with
    | ex -> 
        Console.Error.WriteLine (sprintf "*** %s" ex.Message)
        1

