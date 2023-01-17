﻿namespace PieceRope

open Buffer
open Buffer.Data
open PieceTree.Types
open PieceTree

module PieceRope = 
    let empty = { Tree = PieceTree.Types.PE; Buffer = Buffer.Tree.empty }

    let inline insert index (string: string) piecerope =
        let pcStart = size piecerope.Buffer
        let pcLength = string.Length
        let buffer = Buffer.Tree.append string piecerope.Buffer
        let pt = PieceTree.insert index pcStart pcLength piecerope.Tree
        { Tree = pt; Buffer = buffer }

    let inline delete startIndex length piecerope =
        let tree = PieceTree.PieceTree.delete startIndex length piecerope.Tree
        { Tree = tree; Buffer = piecerope.Buffer }

    let inline substring startIndex length piecerope =
        PieceTree.substring startIndex length piecerope

    type PieceRope with
        member inline this.Insert(index, string) = insert index string this
        member inline this.Delete(startIndex, length) = delete startIndex length this
        member inline this.Substring(startIndex, length) = substring startIndex length this