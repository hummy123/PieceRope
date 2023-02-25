namespace FsRopeBenchmarks

open HumzApps.TextDocument
open Txns

open System
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open BenchmarkDotNet.Jobs

[<MemoryDiagnoser; HtmlExporter; MarkdownExporter>]
type RunTxns() =
  [<Benchmark; IterationCount(100)>]
  member this.Svelte() =
    Utils.runTxns (Sveltecomponent.data)

  [<Benchmark; IterationCount(100)>]
  member this.Rust() =
    Utils.runTxns (Rustcode.data)

  [<Benchmark; IterationCount(100)>]
  member this.Sephblog() =
    Utils.runTxns (Sephblog.data)

  [<Benchmark; IterationCount(100)>]
  member this.Automerge() =
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

module Main = 
    [<EntryPoint>]
    let Main _ =
        BenchmarkRunner.Run<RunTxns>() |> ignore
        BenchmarkRunner.Run<CreateDocument>() |> ignore
        0
