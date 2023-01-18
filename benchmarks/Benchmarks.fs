namespace FsRopeBenchmarks

open PieceRope
open PieceRope.PieceRope

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs


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
        PieceRope.create this.string

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type InsertIntoDocument() =
    [<Params(100, 1000, 10_000)>]
    member val insertTimes = 0 with get, set

    member val rope = PieceRope.empty with get, set
    member val docLength = 0 with get, set

    [<IterationSetup>]
    member this.CreateDocument() =
        this.rope <- PieceRope.empty
        this.docLength <- 0
        for i in [0..this.insertTimes] do
            this.rope <- this.rope.Insert(0, "hello")
            this.docLength <- this.docLength + 5

    [<Benchmark; InvocationCount(1000)>]
    member this.InsertIntoRopeAtStart() = 
        PieceRope.insert 0 "A" this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.InsertIntoRopeAtMiddle() =
        PieceRope.insert (this.docLength / 2) "A" this.rope

    [<Benchmark; InvocationCount(1000)>]
    member this.InsertIntoRopeAtEnd() = 
        PieceRope.insert this.docLength "A" this.rope

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type DeleteFromDocument() =
    [<Params(100, 1000, 10_000)>]
    member val insertTimes = 0 with get, set

    member val rope = PieceRope.empty with get, set
    member val docLength = 0 with get, set

    [<IterationSetup>]
    member this.CreateDocument() =
        this.rope <- PieceRope.empty
        this.docLength <- 0
        for i in [0..this.insertTimes] do
            this.rope <- this.rope.Insert(0, "hello")
            this.docLength <- this.docLength + 5

    [<Benchmark; InvocationCount(1000)>]
    member this.DeleteFromStartOfrope() = 
        this.rope.Delete(0, 10)

    [<Benchmark; InvocationCount(1000)>]
    member this.DeleteFromMiddleOfrope() =
        this.rope.Delete(this.docLength / 2, 10)

    [<Benchmark; InvocationCount(1000)>]
    member this.DeleteFromEndOfrope() = 
        this.rope.Delete(this.docLength - 10, 9)

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type GetSubstring() =
    [<Params(100, 1000, 10_000)>]
    member val insertTimes = 0 with get, set

    member val rope = PieceRope.empty with get, set
    member val docLength = 0 with get, set

    [<IterationSetup>]
    member this.CreateDocument() =
        this.rope <- PieceRope.empty
        this.docLength <- 0
        for i in [0..this.insertTimes] do
            this.rope <- this.rope.Insert(0, "hello")
            this.docLength <- this.docLength + 5

    [<Benchmark; InvocationCount(1000)>]
    member this.GetSubstringAtStartOfrope() = 
        this.rope.Substring(0, 10)

    [<Benchmark; InvocationCount(1000)>]
    member this.GetSubstringAtMiddleOfrope() =
        this.rope.Substring(this.docLength / 2, 10)

    [<Benchmark; InvocationCount(1000)>]
    member this.GetSubstringAtEndOfrope() = 
        this.rope.Substring(this.docLength - 10, 9)

module Main = 
    [<EntryPoint>]
    let Main _ =
        BenchmarkRunner.Run<CreateDocument>() |> ignore
        BenchmarkRunner.Run<InsertIntoDocument>() |> ignore
        BenchmarkRunner.Run<DeleteFromDocument>() |> ignore
        BenchmarkRunner.Run<GetSubstring>() |> ignore
        0