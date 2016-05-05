
#if INTERACTIVE
#r @"..\packages\Microsoft.Diagnostics.Runtime.0.8.31-beta\lib\net40\Microsoft.Diagnostics.Runtime.dll"
#endif
module MiniWinDbg =
    open System
    open Microsoft.Diagnostics.Runtime

    type Object = {
        Id : UInt64
        Type : ClrType
        Fields : Map<string, Val>
        Size : UInt64 }
    and ValueType = {
        Type : ClrType
        //Fields : Map<string, Val option>
        Size : UInt64 }
    and Val =
    | Obj of Object option
    | Boolean of bool
    | Char of char
    | Double of double

    | Struct of ValueType

    type Runtime = {
        Types : ClrType seq
        Objects : Object seq
    }
    [<AutoOpen>]
    module private MiniWinDbg =

        let getValueType t size =
            { Type = t
              Size = uint64 size }

        let rec getObject (heap:ClrHeap) addr =
            let t = heap.GetObjectType addr
            if t = null then None
            else
//                let fields = t.Fields 
//                             |> Seq.map (fun x->x.Name, match x.ElementType with 
//                                                        | ClrElementType.Struct
//                                                        | ClrElementType.Boolean
//                                                        | ClrElementType.Char
//                                                        | ClrElementType.Double
//                                                        | ClrElementType.Float
//                                                        | ClrElementType.Int16
//                                                        | ClrElementType.Int32
//                                                        | ClrElementType.Int64
//                                                        | ClrElementType.Int8
//                                                        | ClrElementType.UInt16
//                                                        | ClrElementType.UInt32
//                                                        | ClrElementType.UInt64
//                                                        | ClrElementType.UInt8
//                                                        | ClrElementType.NativeInt
//                                                        | ClrElementType.NativeUInt
//                                                        | ClrElementType.Pointer ->
//                                                            getValueType x.Type x.Size |> ValType
//                                                        | _ -> 
//                                                            let addr = x.GetAddress(addr, false)
//                                                            getObject heap addr |> Obj)
//                             |> Map.ofSeq
                
            

                { Type = t
                  Size = t.GetSize addr
                  Fields = Map<_,_>[]
                  Id = addr } |> Some
            

        let getObjects (runtimes : ClrRuntime array) = 
            runtimes |> Seq.collect (fun x-> let heap = x.GetHeap()
                                             heap.EnumerateObjectAddresses() |> Seq.map (getObject heap))
                     |> Seq.choose id
    let openDumpFile (path: string) = 
        let target = DataTarget.LoadCrashDump(path, CrashDumpReader.DbgEng)
        let getDac (clrInfo:ClrInfo) = target.SymbolLocator.FindBinary (clrInfo.DacInfo)

        let runtimes = target.ClrVersions |> Seq.map (fun x-> x.CreateRuntime(getDac x)) |> Seq.toArray

        let runtime ={
            Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes())
            Objects = runtimes |> getObjects }


        { new IDisposable with member __.Dispose() = target.Dispose() }, runtime



let d, runtimes = MiniWinDbg.openDumpFile @"C:\memdumps\MHODumpIntegrated\SimCorp.IMS.OM.Client.Application.DMP"
let zz =
    runtimes.Types 
        |> Array.ofSeq 
        |> Array.collect(fun x->x.Methods |> Seq.filter(fun p->p.CompilationType = Microsoft.Diagnostics.Runtime.MethodCompilationType.None) |> Seq.toArray)

let zzz = zz |> Array.filter (fun x ->x.GetFullSignature().Contains "SimCorp.IMS.")


let a = runtimes.Objects 
        |> Array.ofSeq

let group = a |> Array.groupBy (fun x->x.Type.Name) |> Array.Parallel.map(fun (a,b)->a, (b, b |> Array.sumBy (fun p->p.Size))) |> Array.sortByDescending (fun (_,(_,s)) ->s)

