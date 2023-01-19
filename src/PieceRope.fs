namespace PieceRope

open Buffer
open Buffer.Data
open PieceTree.Types
open PieceTree

module PieceRope = 
    let inline findLineBreaks (string: string) =
        let arr = ResizeArray()
        for i = 0 to string.Length - 1 do
            if string[i] = '\n' || string[i] = '\r'
            then arr.Add i
        arr.ToArray()

    let empty = { Tree = PieceTree.Types.PE; Buffer = Buffer.Tree.empty }

    let inline insert index (string: string) piecerope =
        let pcStart = size piecerope.Buffer
        let pcLines = findLineBreaks string
        let buffer = Buffer.Tree.append string piecerope.Buffer
        let pt = PieceTree.insert index pcStart string.Length pcLines piecerope.Tree
        { Tree = pt; Buffer = buffer }

    let inline create string =
        insert 0 string empty

    let inline delete startIndex length piecerope =
        let tree = PieceTree.PieceTree.delete startIndex length piecerope.Tree
        { Tree = tree; Buffer = piecerope.Buffer }

    let inline substring startIndex length piecerope =
        PieceTree.substring startIndex length piecerope

    let inline getLine line piecerope =
        PieceTree.getLine line piecerope

    let inline text piecerope =
        PieceTree.text piecerope

    type PieceRope with
        member inline this.Insert(index, string) = insert index string this
        member inline this.Delete(startIndex, length) = delete startIndex length this
        member inline this.Substring(startIndex, length) = substring startIndex length this
        member inline this.Text() = text this
        member inline this.GetLine(line) = getLine line this