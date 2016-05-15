#if INTERACTIVE
#r @"..\packages\Microsoft.Diagnostics.Runtime.0.8.31-beta\lib\net40\Microsoft.Diagnostics.Runtime.dll"
#load "FSharpDump.fs"
#time "on"
#endif

module Seq =
    open System.Collections.Generic
    let toDict getKey sequence =
        let types = Dictionary()
        for el in sequence do
            let key = getKey el
            match types.TryGetValue key with
            | false,_ -> types.Add(key, ResizeArray [el])
            | true, s -> s.Add el
        types

let d, runtime = FSharpDump.openDumpFile @"C:\tmp\example.dmp"

runtime.Objects |> Seq.skip 10 |> Seq.head

let types = runtime.Objects |> Seq.groupBy(fun p->p.TypeName) |> Map.ofSeq

let p = (types.TryFind "System.Int32[]").Value |> Seq.head

let (FSharpDump.Array pp ) = p

pp.Elements

(*
open System.Diagnostics

let objects = runtime.Objects |> Array.ofSeq

objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.length = 10 |> Debug.Assert
objects |> Array.filter (fun x->x.Type.Name = "example.classA[]") |> Array.length = 1 |> Debug.Assert

let classA = runtime.ObjectsByName "example.classA" |> Seq.toArray

classA.Fields.["structA"].Fields.["c"].Fields

runtime.GCRoots |> Seq.toArray
(*
    * Add support for enums
    * arrays
    * Cancluate distance between 2 objects
    * stacks
    * EnumerateBlockingObjects
    * Delegates on stack
    * JIT information
    * Dead-lock detection
    * BlockingObjects
    * exceptions
    * etc
*)

let p =objects |> Array.filter (fun x->x.Type.Name.StartsWith "System.String[]") |> Array.head



let t = runtime.Threads |> Array.ofSeq*)

