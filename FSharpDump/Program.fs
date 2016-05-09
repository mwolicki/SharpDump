#if INTERACTIVE
#r @"..\packages\Microsoft.Diagnostics.Runtime.0.8.31-beta\lib\net40\Microsoft.Diagnostics.Runtime.dll"
#endif
module MiniWinDbg =
    open System
    open Microsoft.Diagnostics.Runtime

    type Type = {
        Name : string
        Type : unit -> ClrType } with
        override t.ToString() = t.Name

    type Object = {
        Id : UInt64
        Type : Type
        Fields : Map<string, Val>
        Size : UInt64 }
    and ValueType = {
        Type : Type
        Fields : Map<string, Val>
        Size : UInt64 }
    and Val =
    | Obj of Object option
    | SimpleVal of obj
    | Struct of ValueType
    | String of String
    | Lazy of (unit -> Val)

    type Runtime = {
        Types : Type seq
        Objects : Object seq
        GCRoots : Object seq }

    type Val with
        member self.Fields =
            match self with
            | Obj (Some o) -> o.Fields
            | Obj None
            | SimpleVal _
            | String _ -> Map.empty
            | Struct vt -> vt.Fields
            | Lazy v -> v().Fields
        member self.Val =
            match self with
            | SimpleVal o -> Some o
            | String s -> box s |> Some
            | Obj _ | Struct _ | Lazy _ -> None

    [<AutoOpen>]
    module private MiniWinDbg =
        let objCache = System.Collections.Generic.Dictionary()

        let rec getFields (t:ClrType) (addr:uint64) = 
            t.Fields 
            |> Seq.map (fun x->x.Name, match x.ElementType with
                                       | ClrElementType.Int64
                                       | ClrElementType.Boolean
                                       | ClrElementType.Char
                                       | ClrElementType.Double
                                       | ClrElementType.Float
                                       | ClrElementType.Int16
                                       | ClrElementType.Int32
                                       | ClrElementType.Int8
                                       | ClrElementType.UInt16
                                       | ClrElementType.UInt32
                                       | ClrElementType.UInt64
                                       | ClrElementType.UInt8 
                                       | ClrElementType.NativeInt
                                       | ClrElementType.Pointer
                                       | ClrElementType.NativeUInt -> x.GetValue addr |> SimpleVal
                                       | ClrElementType.Struct -> //TODO
                                           getValueType x.Type x.Size addr |> Struct
                                       | ClrElementType.String -> x.GetValue addr :?> string |> String
                                       | ClrElementType.SZArray //TODO
                                       | ClrElementType.Object ->
                                           let addr = x.GetAddress(addr, t.IsValueClass)
                                           match objCache.TryGetValue addr with
                                           | true, v -> v |> Some |> Obj
                                           | false, _ ->
                                                Lazy (fun () -> getObject t.Heap addr |> Obj)
                                       | t -> sprintf "Unsupported type %O" t |> failwith)
            |> Map.ofSeq
        and getValueType t size addr =
            { Type = toType t
              Fields = getFields t addr
              Size = uint64 size }
        and toType t = { Type = (fun () -> t); Name = t.Name }
        and getObject (heap:ClrHeap) addr =
            match objCache.TryGetValue addr with
            | true, v -> Some v
            | false, _ ->
                let t = heap.GetObjectType addr
                if t = null then None
                else
                    let o = 
                        { Type = toType t
                          Size = t.GetSize addr
                          Fields = getFields t addr
                          Id = addr } 
                    objCache.Add (addr, o)
                    o |> Some

        let getObjects (runtimes : ClrRuntime array) = 
            runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                             heap.EnumerateObjectAddresses() |> Seq.map (getObject heap))
                     |> Seq.choose id

    let openDumpFile (path: string) = 
        let target = DataTarget.LoadCrashDump(path, CrashDumpReader.DbgEng)
        let getDac (clrInfo:ClrInfo) = async{
            let! path = target.SymbolLocator.FindBinaryAsync clrInfo.DacInfo |> Async.AwaitTask
            return path, clrInfo }

        let runtimes = target.ClrVersions 
                        |> Seq.map getDac 
                        |> Async.Parallel 
                        |> Async.RunSynchronously
                        |> Array.map(fun (dac, x)-> x.CreateRuntime dac) 

        let runtime = {
            Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes()) |> Seq.map (fun t-> { Name = t.Name; Type = (fun () -> t)})
            Objects = runtimes |> getObjects
            GCRoots = Seq.empty }

        { new IDisposable with member __.Dispose() = target.Dispose() }, runtime 

let d, runtimes = MiniWinDbg.openDumpFile @"C:\tmp\example.dmp"

open System.Diagnostics

let objects = runtimes.Objects |> Array.ofSeq

objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.length = 10 |> Debug.Assert
objects |> Array.filter (fun x->x.Type.Name = "example.classA[]") |> Array.length = 1 |> Debug.Assert

let classA = objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.head

classA.Fields.["structA"].Fields.["c"].Fields


