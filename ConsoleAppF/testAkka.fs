(*
#r "Akka.dll"
#r "Akka.FSharp.dll"
*)

module simple

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

let tid () = Thread.CurrentThread.ManagedThreadId

//set config
let config = ConfigurationFactory.ParseString <| @"
    akka {
        suppress-json-serializer-warning = on
    }"

let system = ActorSystem.Create("FSharp-Akka", config)

//duration for any unit function execution
let duration f =
    let timer = Stopwatch.StartNew()
    let res = f ()
    timer.Stop()
    printfn "... [%d] duration: %i ms" 
        (tid()) timer.ElapsedMilliseconds 
    res

let N = 100000
let countDown = ref N
let mre = new AutoResetEvent(false)

let CreateActors () = 
    [ for i in 1 .. N ->
        spawn system (sprintf "agent_%d" i)
        <| fun (inbox: Actor<string>) ->
            actor {
                let mutable cont = true
                while cont do
                    if i % 50000 = 0 then
                        printfn "    [%d] agent %d receiving ..." (tid()) i
                    
                    let! msg = inbox.Receive()
                    if i % 50000 = 0 then
                        printfn "    [%d] agent %d got message '%s'" (tid()) i msg
                    
                    match msg with 
                    |"ping!" -> ()
                    |"pong!" |_ ->
                        if Interlocked.Decrement(countDown) = 0 then
                            mre.Set() |> ignore
                        cont <- false
                        if i % 50000 = 0 then
                            printfn "    [%d] agent %d got count %d" (tid()) i !countDown
            }
    ]

let test1 () = 
    let s = 10
    let agents = CreateActors()
    for agent in agents do agent.Tell "ping!"
    Thread.Sleep s
    for agent in agents do agent.Tell "ping!"
    Thread.Sleep s
    for agent in agents do agent.Tell "pong!"

    mre.WaitOne() |> ignore // make sure every agent has got Pong message

printfn "... [%d] massive messaging test" (tid ())
duration test1

