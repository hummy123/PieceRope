using System;
using BenchmarkDotNet;
using BenchmarkDotNet.Attributes;
using CrdtBenchmarks;

namespace CrdtBenchmarks
{
    [MemoryDiagnoser, HardwareCounters]
    public class PieceRopeBenchmarks
    {
        [Benchmark]
        public void SvelteBenchmark()
        {
            SvelteComponentBenchmarks.Run();
        }

        [Benchmark]
        public void RustBenchmark()
        {
            RustCodeBenchmarks.Run();
        }

        [Benchmark]
        public void SephBlogBenchmark()
        {
            SephBlog1Benchmarks.Run();
        }

        [Benchmark]
        public void AutomergeBenchmark()
        {
            AutomergePaperBenchmarks.Run();
        }
    }
}
