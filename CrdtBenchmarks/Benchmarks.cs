using System;
using BenchmarkDotNet;
using BenchmarkDotNet.Attributes;
using CrdtBenchmarks;

namespace CrdtBenchmarks
{
    [MemoryDiagnoser]
    public class SvelteBenchmark
    {
        [Benchmark]
        public void Run()
        {
            SvelteComponentBenchmarks.Run();
        }
    }
}
