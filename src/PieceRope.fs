namespace PieceRope

open Buffer
open Buffer.Data
open PieceTree.Types
open PieceTree

module PieceRope = 
    let rec findLineBreaksRec (string: string) strLengthMinus1 pcStart pos (acc: ResizeArray<int>) =
        if pos > strLengthMinus1 then
            if acc.Count > 0 
                then Some <| acc.ToArray()
                else None
        else
            let cur = string[pos]
            if cur = '\n' then
                acc.Add (pos + pcStart)
                findLineBreaksRec string strLengthMinus1 pcStart (pos + 1) acc
            elif cur = '\r' then
                acc.Add (pos + pcStart)
                if pos = strLengthMinus1 then
                    if acc.Count > 0 
                        then Some <| acc.ToArray()
                        else None
                else
                    let next = string[pos + 1]
                    if next = '\n' then
                        findLineBreaksRec string strLengthMinus1 pcStart (pos + 2) acc
                    else
                        findLineBreaksRec string strLengthMinus1 pcStart (pos + 1) acc
            else
                findLineBreaksRec string strLengthMinus1 pcStart (pos + 1) acc

    let empty = { Tree = PieceTree.Types.PE; Buffer = Buffer.Tree.empty }

    let inline insert index (string: string) piecerope =
        let pcStart = size piecerope.Buffer
        let pcLines = findLineBreaksRec string (string.Length - 1) pcStart 0 (ResizeArray())
        let pcLineCount =
            match pcLines with
            | Some x -> x.Length
            | _ -> 0
        let buffer = Buffer.Tree.append string piecerope.Buffer
        let pt = PieceTree.insert index pcStart string.Length pcLines pcLineCount piecerope.Tree
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
