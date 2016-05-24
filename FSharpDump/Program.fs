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

let d, runtime = FSharpDump.openDumpFile @"C:\tmp\example-medium.dmp"


runtime.Objects |> Seq.skip 10 |> Seq.head

let types = runtime.Objects |> Seq.toDict (fun x->x.TypeName)

let p = types.["example.classA"] |> Seq.head
