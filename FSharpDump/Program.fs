
#if INTERACTIVE
#r @"..\packages\Microsoft.Diagnostics.Runtime.0.8.31-beta\lib\net40\Microsoft.Diagnostics.Runtime.dll"
#endif
module MiniWinDbg =
    open System
    open Microsoft.Diagnostics.Runtime

    type Type = {
        Name : string
        Type : ClrType
    }

    type Object = {
        Id : UInt64
        Type : Type
        Fields : unit -> Map<string, Val>
        Size : UInt64 }
    and ValueType = {
        Type : Type
        //Fields : Map<string, Val option>
        Size : UInt64 }
    and Val =
    | Obj of Object option
    | Boolean of bool
    | Char of char
    | Float of float
    | Float32 of float32
    | Int8 of int8
    | Int16 of int16
    | Int32 of int32
    | Int64 of int64
    | UInt8 of uint8
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | NativeInt of nativeint
    | UNativeInt of unativeint
    | Struct of ValueType

    type Runtime = {
        Types : Type seq
        Objects : Object seq }

    [<AutoOpen>]
    module private MiniWinDbg =

        let getValueType t size =
            { Type = t
              Size = uint64 size }
        let toType t = { Type = t; Name = t.Name }
        let rec getObject (heap:ClrHeap) addr =
            let t = heap.GetObjectType addr
            if t = null then None
            else
                let field = fun () -> t.Fields 
                                      |> Seq.map (fun x->x.Name, match x.ElementType with 

                                                                 | ClrElementType.Int64 -> x.GetValue addr :?> int64 |> Int64
                                                                 | ClrElementType.Boolean -> x.GetValue addr :?> bool |> Boolean
                                                                 | ClrElementType.Char -> x.GetValue addr :?> char |> Char
                                                                 | ClrElementType.Double -> x.GetValue addr :?> float32 |> Float32
                                                                 | ClrElementType.Float -> x.GetValue addr :?> float |> Float
                                                                 | ClrElementType.Int16 -> x.GetValue addr :?> int16 |> Int16
                                                                 | ClrElementType.Int32 -> x.GetValue addr :?> int32 |> Int32
                                                                 | ClrElementType.Int8 -> x.GetValue addr :?> int8 |> Int8
                                                                 | ClrElementType.UInt16 -> x.GetValue addr :?> uint16 |> UInt16
                                                                 | ClrElementType.UInt32 -> x.GetValue addr :?> uint32 |> UInt32
                                                                 | ClrElementType.UInt64 -> x.GetValue addr :?> uint64 |> UInt64
                                                                 | ClrElementType.UInt8 -> x.GetValue addr :?> uint8 |> UInt8
                                                                 | ClrElementType.NativeInt -> x.GetValue addr :?> nativeint |> NativeInt
                                                                 | ClrElementType.NativeUInt -> x.GetValue addr :?> unativeint |> UNativeInt
                                                                 | ClrElementType.Struct
                                                                 | ClrElementType.Pointer ->
                                                                     getValueType (toType x.Type) x.Size |> Struct
                                                                 | _ -> 
                                                                     let addr = x.GetAddress(addr, false)
                                                                     getObject heap addr |> Obj)
                                      |> Map.ofSeq

                { Type = toType t
                  Size = t.GetSize addr
                  Fields = field
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
            Types = runtimes |> Seq.collect (fun x->x.GetHeap().EnumerateTypes()) |> Seq.map (fun t-> { Name = t.Name; Type = t})
            Objects = runtimes |> getObjects }


        { new IDisposable with member __.Dispose() = target.Dispose() }, runtime



let d, runtimes = MiniWinDbg.openDumpFile @"C:\tmp\example.dmp"

open System.Diagnostics

let objects = runtimes.Objects |> Array.ofSeq

objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.length = 10 |> Debug.Assert
objects |> Array.filter (fun x->x.Type.Name = "example.classA[]") |> Array.length = 1 |> Debug.Assert

let classA = objects |> Array.filter (fun x->x.Type.Name = "example.classA") |> Array.head
classA.Fields().["Long"]



let o = objects |> Array.filter (fun x->x.Type.Name.Contains "classA")


