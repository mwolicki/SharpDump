#if INTERACTIVE
#r @"..\packages\Microsoft.Diagnostics.Runtime.0.8.31-beta\lib\net40\Microsoft.Diagnostics.Runtime.dll"
#time "on"
#endif
module MiniWinDbg =
    open System
    open Microsoft.Diagnostics.Runtime

    type ObjectRef = UInt64
    type TypeName = string

    type Type =
        struct
            val ClrType : ClrType
            val private _fields : ClrType -> ObjectRef -> Map<string, unit -> Val>
            member t.Fields = t._fields t.ClrType
            member t.Name : TypeName = t.ClrType.Name
            override t.ToString() = t.Name
            new (clrType, fields) = { ClrType = clrType; _fields = fields }
        end
    and Object = 
        struct
            val Id : ObjectRef 
            val Type : Type
            member o.Size = o.Type.ClrType.GetSize o.Id
            member o.Fields with get () = o.Type.Fields o.Id

            override o.ToString() = sprintf "{ Id = %i; Type = %s}" o.Id o.Type.Name

            new (id, type') = {Id = id; Type = type'}
        end
    
    and ValueType = 
        struct
            val Id : ObjectRef 
            val Type : Type
            val Size : Int32
            member o.Fields with get () = o.Type.Fields o.Id
            override o.ToString() = sprintf "{ Id = %i; Type = %s; Size = %i}" o.Id o.Type.Name o.Size

            new (id, type', size) = {Id = id; Type = type'; Size = size}
        end
    and Arr = 
        struct
            val Id : ObjectRef
            val Type : Type
            member o.Length = o.Type.ClrType.GetArrayLength o.Id
            member o.Elements = 
                let len = o.Length
                let t = o.Type.ClrType
                let id = o.Id
                let getObject = o._getObject
                seq { for i = 0 to len do
                        yield t.GetArrayElementAddress(id, i) |> getObject t.Heap }
            val private _getObject : ClrHeap -> ObjectRef -> Val
            override o.ToString() = sprintf "{ Elements = %A; Type = %s; Size = %i}" o.Elements o.Type.Name o.Length

            new (id, getObject, type') = {Id = id; _getObject = getObject; Type = type'}
        end
    and Val =
    | Obj of Object
    | SimpleVal of obj
    | Struct of ValueType
    | Str of String
    | Array of Arr
    | Null
     with
        member self.Fields =
            match self with
            | Obj o ->  o.Fields
            | SimpleVal _
            | Array _
            | Str _ -> Map.empty
            | Null -> Map.empty
            | Struct vt -> vt.Fields

        member self.Val =
            match self with
            | SimpleVal o -> o
            | Str s -> box s
            | Obj _ | Struct _ | Array _ | Null -> obj()
            (*
        member self.Type =
            match self with
            | Str o -> Type(ClrType(), fun _ -> Map.empty)
            | SimpleVal o -> Type()
            | Obj o -> o.Type
            | Null -> Type()
            | Struct s -> s.Type
            | Array (_, t, _) -> t*)

    [<Flags>]
    type ThreadFlag =
    | None                          = 0
    | AbortRequested                = 1
    | Aborted                       = 2
    | Alive                         = 4
    | Background                    = 8
    | CoInitialized                 = 16
    | DebugSuspended                = 32
    | DebuggerHelper                = 64
    | Finalizer                     = 128
    | GC                            = 256
    | GCSuspendPending              = 512
    | MTA                           = 1024
    | STA                           = 2048
    | ShutdownHelper                = 4096
    | SuspendingEE                  = 8192
    | ThreadpoolCompletionPort      = 16384
    | ThreadpoolGate                = 32768
    | ThreadpoolTimer               = 65536
    | ThreadpoolWait                = 131072
    | ThreadpoolWorker              = 262144
    | Unstarted                     = 524288
    | UserSuspended                 = 1048576

    type Thread = {
        Stack : string list
        Id : uint32
        ManagedThreadId : int
        ThreadObj : Object option
        //BlockingObjects
        LockCount : uint32
        Name : string
        Flags : ThreadFlag }

    type Runtime = {
        Types : Type seq
        Objects : Val seq
        ObjectsByName : TypeName -> Val seq
        GCRoots: Val seq
        ///Objects which have been collected, but are awaiting for Finalizer
        FinalizableQueue : Val seq
        Threads : Thread seq }


    [<AutoOpen>]
    module private MiniWinDbg =
        let rec getFields (t:ClrType) (addr:uint64) = 
            t.Fields 
            |> Seq.map (fun x->x.Name, 
                               fun () -> match x.ElementType with
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
                                             x.GetAddress(addr, t.IsValueClass)
                                             |> getObject t.Heap
                                         | t -> sprintf "Unsupported type %O" t |> failwith)
            |> Map.ofSeq
        and getFieldsCached = 
            () //force cache
            getFields
        and getValueType t size addr =
            ValueType(addr, Type(t, getFieldsCached), size)
        and getObject (heap:ClrHeap) addr : Val =
                let t = heap.GetObjectType addr
                if t = null then Null 
                else
                    if t.IsString then
                        t.GetValue addr :?> string |> Str
                    elif t.IsArray then
                        Arr (addr, getObjectCached, Type (t, getFieldsCached)) |> Array
                    else
                        let refersObjs () = 
                            let a = ResizeArray()
                            let iter (addr:uint64) _ = getObject heap addr |> a.Add
                            t.EnumerateRefsOfObjectCarefully (addr, Action<uint64, int>(iter))
                            a.ToArray()
                        Object(addr, Type(t, getFieldsCached)) |> Obj
        and getObjectCached =
            ()
            fun (heap:ClrHeap) (addr:ObjectRef)-> getObject heap addr
        let getObjects (runtimes : ClrRuntime array) = 
            runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                             heap.EnumerateObjectAddresses() |> Seq.map (getObject heap))
        let getObjectsByName (runtimes : ClrRuntime array) name = 
            runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                             heap.EnumerateObjectAddresses()
                                             |> Seq.filter (fun ref -> (heap.GetObjectType ref).Name = name)
                                             |> Seq.map (getObject heap))

        let getRootObjects (runtimes : ClrRuntime array)  =
            let getRootObjects (heap:ClrHeap) =
                heap.EnumerateRoots() 
                |> Seq.map (fun root -> getObject heap root.Object)
            runtimes |> Seq.collect (fun x -> getRootObjects (x.GetHeap()))

        let getFinalizableQueue (runtimes : ClrRuntime array) =
            let getFinalizableQueue (heap:ClrHeap) =
                heap.EnumerateFinalizableObjectAddresses() 
                |> Seq.map (getObject heap)
            runtimes |> Seq.collect (fun x -> getFinalizableQueue (x.GetHeap()))

        let getThreadFlag (t: ClrThread) =
            let (||||) (f:ThreadFlag) (b, flag:ThreadFlag) = 
                if b then f ||| flag
                else f
            ThreadFlag.None
            |||| (t.IsAbortRequested, ThreadFlag.AbortRequested)
            |||| (t.IsAborted, ThreadFlag.Aborted)
            |||| (t.IsAlive, ThreadFlag.Alive)
            |||| (t.IsBackground, ThreadFlag.Background)
            |||| (t.IsCoInitialized, ThreadFlag.CoInitialized)
            |||| (t.IsDebugSuspended, ThreadFlag.DebugSuspended)
            |||| (t.IsDebuggerHelper, ThreadFlag.DebuggerHelper)
            |||| (t.IsFinalizer, ThreadFlag.Finalizer)
            |||| (t.IsGC, ThreadFlag.GC)
            |||| (t.IsGCSuspendPending, ThreadFlag.GCSuspendPending)
            |||| (t.IsMTA, ThreadFlag.MTA)
            |||| (t.IsSTA, ThreadFlag.STA)
            |||| (t.IsShutdownHelper, ThreadFlag.ShutdownHelper)
            |||| (t.IsSuspendingEE, ThreadFlag.SuspendingEE)
            |||| (t.IsThreadpoolCompletionPort, ThreadFlag.ThreadpoolCompletionPort)
            |||| (t.IsThreadpoolGate, ThreadFlag.ThreadpoolGate)
            |||| (t.IsThreadpoolTimer, ThreadFlag.ThreadpoolTimer)
            |||| (t.IsThreadpoolWait, ThreadFlag.ThreadpoolWait)
            |||| (t.IsThreadpoolWorker, ThreadFlag.ThreadpoolWorker)
            |||| (t.IsUnstarted, ThreadFlag.Unstarted)
            |||| (t.IsUserSuspended, ThreadFlag.UserSuspended)

        let getThreads (runtimes : ClrRuntime array) =
            let threadsObjs = lazy( 
                getObjectsByName runtimes "System.Threading.Thread"
                |> Seq.choose (function Obj o -> Some o | _ -> None)
                |> Seq.map (fun x->unbox<int> (x.Fields.["m_ManagedThreadId"]()).Val, x)
                |> Map.ofSeq)

            let getThreads (runtime: ClrRuntime) =
                runtime.Threads 
                |> Seq.map (fun t-> 
                    let threadObj =  threadsObjs.Value.TryFind t.ManagedThreadId
                    let name = 
                        match threadObj with 
                        | Some o ->
                            match o.Fields.["m_Name"]()  with
                            | Str s -> s
                            | _ -> ""
                        | _ -> ""

                    { ThreadObj = threadObj; LockCount = t.LockCount; Name = name; ManagedThreadId = t.ManagedThreadId; Stack = []; Id = t.OSThreadId; Flags = getThreadFlag t})
            runtimes |> Seq.collect getThreads

        let getRuntime (target:DataTarget) = 
            let getDac (clrInfo:ClrInfo) = async{
                let! path = target.SymbolLocator.FindBinaryAsync clrInfo.DacInfo |> Async.AwaitTask
                return path, clrInfo }

            let runtimes = target.ClrVersions 
                            |> Seq.map getDac 
                            |> Async.Parallel 
                            |> Async.RunSynchronously
                            |> Array.map(fun (dac, x)-> x.CreateRuntime dac) 

            let runtime =
              { Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes()) |> Seq.map (fun t -> Type(t, getFieldsCached))
                Objects = runtimes |> getObjects
                ObjectsByName = runtimes |> getObjectsByName
                GCRoots = runtimes |> getRootObjects
                FinalizableQueue = runtimes |> getFinalizableQueue
                Threads = runtimes |> getThreads }

            { new IDisposable with member __.Dispose() = target.Dispose() }, runtime 

    let openDumpFile (path: string) = DataTarget.LoadCrashDump(path, CrashDumpReader.DbgEng) |> getRuntime

    let attach pid = DataTarget.AttachToProcess(pid, 30000u, AttachFlag.Invasive) |> getRuntime

module Seq =
    open System.Collections.Generic
    let toDict (getKey) (sequence) =
        let types = Dictionary()
        for el in sequence do
            let key = getKey el
            match types.TryGetValue key with
            | false,_ -> types.Add(key, ResizeArray [el])
            | true, s -> s.Add el
        types

let d, runtime = MiniWinDbg.openDumpFile @"C:\tmp\example-medium.dmp"


let types = runtime.Threads |> Seq.toArray

(*
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
    * BlockingObjects
    * exceptions
    * etc
*)

let p =objects |> Array.filter (fun x->x.Type.Name.StartsWith "System.String[]") |> Array.head



let t = runtime.Threads |> Array.ofSeq*)

