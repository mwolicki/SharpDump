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
    | String of String
    | Lazy of (unit -> Val option)

    type Runtime = {
        Types : Type seq
        Objects : Val seq
        GCRoots: Val seq }

    type Val with
        member self.Fields =
            match self with
            | Obj o -> o.Fields
            | SimpleVal _
            | String _ -> Map.empty
            | Struct vt -> vt.Fields
            | Lazy v -> 
                let v = v()
                match v with
                | Some s->s.Fields
                | None -> Map.empty
        member self.Val =
            match self with
            | SimpleVal o -> Some o
            | String s -> box s |> Some
            | Obj _ | Struct _ | Lazy _ -> None

        member self.Type =
            match self with
            | String o -> { Name = o.GetType().FullName; Type = fun () -> Unchecked.defaultof<ClrType>}
            | SimpleVal o -> { Name = o.GetType().FullName; Type = fun () -> Unchecked.defaultof<ClrType>}
            | Obj o -> o.Type
            | Struct s -> s.Type
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
                                       | ClrElementType.String -> x.GetValue addr :?> string |> String
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
                        t.GetValue addr :?> string |> (String >> Some)
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

    let getRootObjects (heap:ClrHeap) =
        heap.EnumerateRoots() 
        |> Seq.map (fun root -> getObject heap root.Object)
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

        let runtime =
          { Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes()) |> Seq.map (fun t-> { Name = t.Name; Type = (fun () -> t)})
            Objects = runtimes |> getObjects
            GCRoots = runtimes |> Seq.collect (fun x-> getRootObjects (x.GetHeap())) }

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
    * etc
*)

objects |> Array.filter (fun x->x.Type.Name = "System.String")
