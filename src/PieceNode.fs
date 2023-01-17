﻿namespace PieceTree

open PieceTree.Types

module Types =
    let inline create start length = 
        { Start = start; Length = length; LeftIdx = 0; RightIdx = 0; }

    let inline addLeft leftDelta node =
        { node with LeftIdx = node.LeftIdx + leftDelta }

    let inline addRight rightDelta node =
        { node with RightIdx = node.RightIdx + rightDelta }

    let inline setIdx left right node =
        { node with LeftIdx = left; RightIdx = right; }
