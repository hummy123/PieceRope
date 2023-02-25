namespace FsRopeBenchmarks

open HumzApps.TextDocument
open Txns

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type SvelteTxns() =
  [<Benchmark; IterationCount(1000)>]
  member this.Run() =
    Utils.runTxns (Sveltecomponent.data)

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type RustTxns() =
  [<Benchmark; IterationCount(1000)>]
  member this.Run() =
    Utils.runTxns (Rustcode.data)

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type SephblogTxns() =
  [<Benchmark; IterationCount(1000)>]
  member this.Run() =
    Utils.runTxns (Sephblog.data)

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type AutomergeTxns() =
  [<Benchmark; IterationCount(1000)>]
  member this.Run() =
    Utils.runTxns (Automerge.data)

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type CreateDocument() =
    [<Params(100, 1_000, 10_000)>]
    member val stringLength = 0 with get, set
    
    member val string = "" with get, set

    [<IterationSetup>]
    member this.BuildString() =
        this.string <- String.replicate this.stringLength "a"

    [<Benchmark; InvocationCount(1000)>]
    member this.CreateRopeOfSize() = 
        TextDocument.create this.string

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type InsertIntoDocument() =
    [<Params(100, 1000, 10_000)>]
    member val insertTimes = 0 with get, set

    member val rope = TextDocument.empty with get, set
    member val docLength = 0 with get, set

    [<IterationSetup>]
    member this.CreateDocument() =
        let str = String.replicate this.insertTimes "hello"
        this.docLength <- str.Length
        this.rope <- TextDocument.create str

    [<Benchmark; InvocationCount(1000)>]
    member this.InsertIntoRopeAtStart() = 
        TextDocument.insert 0 "A" this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.InsertIntoRopeAtMiddle() =
        TextDocument.insert (this.docLength / 2) "A" this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.InsertIntoRopeAtEnd() = 
        TextDocument.insert this.docLength "A" this.rope

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type DeleteFromDocument() =
    [<Params(100, 1000, 10_000)>]
    member val insertTimes = 0 with get, set

    member val rope = TextDocument.empty with get, set
    member val docLength = 0 with get, set

    [<IterationSetup>]
    member this.CreateDocument() =
        let str = String.replicate this.insertTimes "hello"
        this.docLength <- str.Length
        this.rope <- TextDocument.create str

    [<Benchmark; InvocationCount(1000)>]
    member this.DeleteFromStartOfrope() = 
        TextDocument.delete 0 1 this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.DeleteFromMiddleOfrope() =
        TextDocument.delete (this.docLength / 2) 10 this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.DeleteFromEndOfrope() = 
        TextDocument.delete (this.docLength - 10) 9 this.rope

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type GetSubstring() =
    [<Params(100, 1000, 10_000)>]
    member val insertTimes = 0 with get, set

    member val rope = TextDocument.empty with get, set
    member val docLength = 0 with get, set

    [<IterationSetup>]
    member this.CreateDocument() =
        let str = String.replicate this.insertTimes "hello"
        this.docLength <- str.Length
        this.rope <- TextDocument.create str

    [<Benchmark; InvocationCount(1000)>]
    member this.GetSubstringAtStartOfrope() = 
        TextDocument.substring 0 10 this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.GetSubstringAtMiddleOfrope() =
        TextDocument.substring (this.docLength / 2) 10 this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.GetSubstringAtEndOfrope() = 
        TextDocument.substring (this.docLength - 10) 9 this.rope

module Main = 
    [<EntryPoint>]
    let Main _ =
        BenchmarkRunner.Run<SvelteTxns>() |> ignore
        BenchmarkRunner.Run<RustTxns>() |> ignore
        BenchmarkRunner.Run<SephblogTxns>() |> ignore
        BenchmarkRunner.Run<AutomergeTxns>() |> ignore

        BenchmarkRunner.Run<CreateDocument>() |> ignore
        BenchmarkRunner.Run<InsertIntoDocument>() |> ignore
        BenchmarkRunner.Run<DeleteFromDocument>() |> ignore
        BenchmarkRunner.Run<GetSubstring>() |> ignore
        0
