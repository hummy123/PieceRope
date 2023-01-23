const fs = require("fs")
const path = require("path")

const filename = "sveltecomponent"
const ClassName = "SvelteComponent"

const inputPath = path.resolve(`${__dirname}\\${filename}.json`)

const {
  txns
} = JSON.parse(fs.readFileSync(inputPath, 'utf-8'))

let content = `
using System;
using BenchmarkDotNet;
using BenchmarkDotNet.Attributes;
using PieceRope;
using static PieceRope.PieceRope;

namespace CrdtBenchmarks
{
    public static class ${ClassName}Benchmarks
    {
        public static void Run()
        {
            var rope = PieceRope.PieceRope.empty;
`

for (const entry of txns) {
    for (const [pos, delNum, insStr] of entry.patches) {
        if (delNum > 0) {
            content += `rope = PieceRope.PieceRope.delete(${pos}, ${delNum}, rope);\n`
        }
        if (insStr !== "") {
            content += `rope = PieceRope.PieceRope.insert(${pos}, "${escape(insStr)}", rope);\n`
        }
    }
}

content += `
        }
    }
}
`

let outputPath = path.resolve(`${__dirname}\\${ClassName}.cs`)

fs.writeFileSync(outputPath, content)