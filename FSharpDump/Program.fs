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
        Size : UInt64
        RefersObjs : unit -> Val array }

    and ValueType = {
        Type : Type
        Fields : Map<string, Val>
        Size : UInt64 }

    and Val =
    | Obj of Object
    | SimpleVal of obj
    | Struct of ValueType
    | Str of String
    | Array of Val option seq * Type * size:int
    | Lazy of (unit -> Val option)

    type Thread = {
        Stack : string list
        Id : uint32
        ManagedThreadId : int
        ThreadObj:Object option
        Name : string }

    type Runtime = {
        Types : Type seq
        Objects : Val seq
        GCRoots: Val seq
        ///Objects which have been collected, but are awaiting for Finalizer
        FinalizableQueue : Val seq
        Threads : Thread seq }


    type Val with
        member self.Fields =
            match self with
            | Obj o -> o.Fields
            | SimpleVal _
            | Array _
            | Str _ -> Map.empty
            | Struct vt -> vt.Fields
            | Lazy v -> 
                let v = v()
                match v with
                | Some s->s.Fields
                | None -> Map.empty
        member self.Val =
            match self with
            | SimpleVal o -> Some o
            | Str s -> box s |> Some
            | Obj _ | Struct _ | Lazy _ | Array _ -> None

        member self.Type =
            match self with
            | Str o -> { Name = o.GetType().FullName; Type = fun () -> Unchecked.defaultof<ClrType>}
            | SimpleVal o -> { Name = o.GetType().FullName; Type = fun () -> Unchecked.defaultof<ClrType>}
            | Obj o -> o.Type
            | Struct s -> s.Type
            | Array (_, t, _) -> t
            | Lazy v -> 
                let v = v()
                match v with
                | Some s->s.Type
                | None ->  { Name = "(unknown type)"; Type = fun () -> Unchecked.defaultof<ClrType>}

    [<AutoOpen>]
    module private MiniWinDbg =
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
                                       | ClrElementType.Struct ->
                                           getValueType x.Type x.Size addr |> Struct
                                       | ClrElementType.String -> x.GetValue addr :?> string |> Str
                                       | ClrElementType.SZArray
                                       | ClrElementType.Object ->
                                           let addr = x.GetAddress(addr, t.IsValueClass)
                                           Lazy (fun () -> getObject t.Heap addr)
                                       | t -> sprintf "Unsupported type %O" t |> failwith)
            |> Map.ofSeq
        and getValueType t size addr =
            { Type = toType t
              Fields = getFields t addr
              Size = uint64 size }
        and toType t = { Type = (fun () -> t); Name = t.Name }
        and getObject (heap:ClrHeap) addr : Val option =
                let t = heap.GetObjectType addr
                if t = null then None 
                else
                    if t.IsString then
                        t.GetValue addr :?> string |> (Str >> Some)
                    elif t.IsArray then
                        let len = t.GetArrayLength addr
                        seq { for i = 0 to len do
                                yield t.GetArrayElementAddress(addr, i) |> getObject heap }
                        |> fun el ->  Array (el, toType t, len)
                        |> Some
                    else
                        let refersObjs () = 
                            let a = ResizeArray()
                            let iter (addr:uint64) _ = 
                                match getObject heap addr with
                                | Some s-> s |> a.Add
                                | None -> ()
                            t.EnumerateRefsOfObjectCarefully (addr, Action<uint64, int>(iter))
                            a.ToArray()
                        { Type = toType t
                          Size = t.GetSize addr
                          Fields = getFields t addr
                          Id = addr
                          RefersObjs =  refersObjs} |> (Obj >> Some)

        let getObjects (runtimes : ClrRuntime array) = 
            runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                             heap.EnumerateObjectAddresses() |> Seq.map (getObject heap))
                     |> Seq.choose id


        let getRootObjects (runtimes : ClrRuntime array)  =
            let getRootObjects (heap:ClrHeap) =
                heap.EnumerateRoots() 
                |> Seq.map (fun root -> getObject heap root.Object)
                |> Seq.choose id
            runtimes |> Seq.collect (fun x -> getRootObjects (x.GetHeap()))

        let getFinalizableQueue (runtimes : ClrRuntime array) =
            let getFinalizableQueue (heap:ClrHeap) =
                heap.EnumerateFinalizableObjectAddresses() 
                |> Seq.map (getObject heap)
                |> Seq.choose id
            runtimes |> Seq.collect (fun x -> getFinalizableQueue (x.GetHeap()))

        let getThreads (runtimes : ClrRuntime array) =
            let getThreads (runtime: ClrRuntime) =

                let threadObj (id:int) = 
                    getObjects runtimes 
                    |> Seq.filter(fun x->x.Type.Name = "System.Threading.Thread")
                    |> Seq.choose (function Obj o -> Some o | _ -> None)
                    |> Seq.tryFind(fun x->(unbox x.Fields.["m_ManagedThreadId"].Val.Value) = id)

                runtime.Threads 
                |> Seq.map (fun t-> 
                    let threadObj = threadObj t.ManagedThreadId
                    let name = 
                        match threadObj with 
                        | Some o ->
                            match o.Fields.["m_Name"]  with
                            | Str s -> s
                            | _ -> ""
                        | _ -> ""
                    { ThreadObj = threadObj; Name = name; ManagedThreadId = t.ManagedThreadId; Stack = []; Id = t.OSThreadId})
            runtimes 
            |> Seq.collect getThreads


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

        let runtime =
          { Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes()) |> Seq.map (fun t-> { Name = t.Name; Type = (fun () -> t)})
            Objects = runtimes |> getObjects
            GCRoots = runtimes |> getRootObjects
            FinalizableQueue = runtimes |> getFinalizableQueue
            Threads = runtimes |> getThreads }

        { new IDisposable with member __.Dispose() = target.Dispose() }, runtime 

let d, runtime = MiniWinDbg.openDumpFile @"C:\tmp\example.dmp"

open System.Diagnostics

let objects = runtime.Objects |> Array.ofSeq

objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.length = 10 |> Debug.Assert
objects |> Array.filter (fun x->x.Type.Name = "example.classA[]") |> Array.length = 1 |> Debug.Assert

let classA = objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.head

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
    * etc
*)

let p =objects |> Array.filter (fun x->x.Type.Name.StartsWith "System.String[]") |> Array.head


let t = runtime.Threads |> Seq.head

