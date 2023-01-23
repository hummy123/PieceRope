using BenchmarkDotNet.Running;
using CrdtBenchmarks;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PieceRopeCrdtBenchmarks
{
    class Program
    {
        static void Main(string[] args)
        {
            var summary = BenchmarkRunner.Run<PieceRopeBenchmarks>();
        }

    }
}
