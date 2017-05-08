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

[<EntryPoint>]
let main args = 
    printfn "... command line args: %A" args

    let fname = args.[0]
    let M = Data.getData fname
    printfn "input: %A" M
    
    0 // return an integer exit code

