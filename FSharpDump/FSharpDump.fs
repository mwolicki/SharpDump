module FSharpDump

open System
open Microsoft.Diagnostics.Runtime

type ObjectRef = UInt64
type TypeName = string

let private (|IsStruct|IsString|IsSimpleVal|IsObject|IsArray|) =
    function
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
        | ClrElementType.NativeUInt -> IsSimpleVal
        | ClrElementType.Struct -> IsStruct
        | ClrElementType.String -> IsString
        | ClrElementType.SZArray -> IsArray
        | ClrElementType.Object -> IsObject
        | t -> sprintf "Unsupported type %O" t |> failwith

type Type =
    struct
        val ClrType : ClrType
        member t.Fields = Tools.getFields t.ClrType
        member t.Name : TypeName = t.ClrType.Name
        override t.ToString() = t.Name
        new (clrType) = { ClrType = clrType }
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
            seq { for i = 0 to len do
                    yield t.GetArrayElementAddress(id, i) |> Tools.getObject t.ComponentType }
        override o.ToString() = sprintf "{ Elements = %A; Type = %s; Size = %i}" o.Elements o.Type.Name o.Length

        new (id, type') = {Id = id; Type = type'}
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

    member self.Elements =
        match self with
        | Array arr -> arr.Elements
        | _ -> Seq.empty

    member self.Val =
        match self with
        | SimpleVal o -> o
        | Str s -> box s
        | Obj _ | Struct _ | Array _ | Null -> obj()
         
    member self.TypeName =
        match self with
        | Str o -> typeof<string>.FullName
        | SimpleVal o -> o.GetType().FullName
        | Obj o -> o.Type.Name
        | Null -> System.String.Empty
        | Struct s -> s.Type.Name
        | Array arr -> arr.Type.Name
and
    [<AbstractClass; Sealed>] private Tools private () =
    static member getFields (t:ClrType) (addr:uint64) = 
        t.Fields 
        |> Seq.map (fun x->x.Name, 
                            lazy(match x.ElementType with
                                        | IsSimpleVal -> x.GetValue addr |> SimpleVal
                                        | IsStruct -> Tools.getValueType x.Type x.Size addr |> Struct
                                        | IsString-> x.GetValue addr :?> string |> Str
                                        | IsArray
                                        | IsObject ->
                                            x.GetAddress(addr, t.IsValueClass)
                                            |> Tools.getObject (t.Heap.GetObjectType addr)))
        |> Map.ofSeq
    static member getValueType t size addr =
        ValueType(addr, Type t, size)
    static member getObject (t:ClrType) addr : Val =
            if t = null then Null 
            else
                match t.ElementType with
                | IsSimpleVal -> t.GetValue addr |> SimpleVal
                | IsStruct -> Tools.getValueType t (int <| t.GetSize addr) addr |> Struct
                | IsString-> t.GetValue addr :?> string |> Str
                | IsArray ->
                    Arr (addr, Type t) |> Array
                | IsObject ->
                    Object(addr, Type t) |> Obj

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
    ObjectsByTypeName : TypeName -> Val seq
    GCRoots: Val seq
    ///Objects which have been collected, but are awaiting for Finalizer
    FinalizableQueue : Val seq
    Threads : Thread seq }


[<AutoOpen>]
module private MiniWinDbg =

        
    let getObjects (runtimes : ClrRuntime array) = 
        runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                         heap.EnumerateObjectAddresses() |> Seq.map (fun addr-> Tools.getObject (heap.GetObjectType addr) addr))
    let getObjectsByName (runtimes : ClrRuntime array) name = 
        runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                         heap.EnumerateObjectAddresses()
                                         |> Seq.filter (fun ref -> (heap.GetObjectType ref).Name = name)
                                         |> Seq.map (fun addr -> Tools.getObject (heap.GetObjectType addr) addr))

    let getRootObjects (runtimes : ClrRuntime array)  =
        let getRootObjects (heap:ClrHeap) =
            heap.EnumerateRoots() 
            |> Seq.map (fun root -> Tools.getObject (heap.GetObjectType root.Object)  root.Object)
        runtimes |> Seq.collect (fun x -> getRootObjects (x.GetHeap()))

    let getFinalizableQueue (runtimes : ClrRuntime array) =
        let getFinalizableQueue (heap:ClrHeap) =
            heap.EnumerateFinalizableObjectAddresses() 
            |> Seq.map (fun addr -> Tools.getObject (heap.GetObjectType addr) addr)
        runtimes |> Seq.collect (fun x -> getFinalizableQueue (x.GetHeap()))

    let getThreadFlag (t: ClrThread) =
        let inline (||||) (f:ThreadFlag) (enabled, flag:ThreadFlag) = if enabled then f ||| flag else f
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
            |> Seq.map (fun x->unbox<int> x.Fields.["m_ManagedThreadId"].Value.Val, x)
            |> Map.ofSeq)

        let getThreads (runtime: ClrRuntime) =
            runtime.Threads 
            |> Seq.map (fun clrType-> 
                let threadObj =  threadsObjs.Value.TryFind clrType.ManagedThreadId

                let name = 
                    match threadObj with 
                    | Some o ->
                        match o.Fields.["m_Name"].Value with
                        | Str s -> s
                        | _ -> ""
                    | _ -> ""

                { ThreadObj = threadObj
                  LockCount = clrType.LockCount
                  Name = name
                  ManagedThreadId = clrType.ManagedThreadId
                  Stack =  [ for frame in clrType.StackTrace -> frame.DisplayString ]
                  Id = clrType.OSThreadId
                  Flags = getThreadFlag clrType})
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
            { Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes()) |> Seq.map (fun t -> Type t)
              Objects = runtimes |> getObjects
              ObjectsByTypeName = runtimes |> getObjectsByName
              GCRoots = runtimes |> getRootObjects
              FinalizableQueue = runtimes |> getFinalizableQueue
              Threads = runtimes |> getThreads }

        { new IDisposable with member __.Dispose() = target.Dispose() }, runtime 

let openDumpFile (path: string) = DataTarget.LoadCrashDump(path, CrashDumpReader.DbgEng) |> getRuntime

let attach pid = 
    let dataTarget = DataTarget.AttachToProcess(pid, 30000u, AttachFlag.Invasive) 
    dataTarget, dataTarget |> getRuntime