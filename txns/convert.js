const fs = require("fs")
const path = require("path")

const filename = "rustcode"
const moduleName = "Rustcode"

const inputPath = path.resolve(`./${filename}.json`)

const {
  txns
} = JSON.parse(fs.readFileSync(inputPath, 'utf-8'))

let content = `
module ${moduleName}

let data = [|
`

for (const entry of txns) {
    for (const [pos, delNum, insStr] of entry.patches) {
      const str = JSON.stringify(insStr) // escape special chars in string
    	content += `  (${pos}, ${delNum}, ${str});\n`
    }
}

content += `
|]
`

let outputPath = `${filename}.fs`

fs.writeFileSync(outputPath, content)
