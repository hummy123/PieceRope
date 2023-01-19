namespace PieceTree

open PieceTree.Types

module Node =
    let inline create start length = 
        { Start = start; Length = length; LeftIdx = 0; RightIdx = 0; }

    let inline addLeft leftDelta node =
        { node with LeftIdx = node.LeftIdx + leftDelta }

    let inline addRight rightDelta node =
        { node with RightIdx = node.RightIdx + rightDelta }

    let inline setIdx left right node =
        { node with LeftIdx = left; RightIdx = right; }

    type PieceNode with
        member inline this.AddLeft leftDelta = addLeft leftDelta this
        member inline this.AddRight rightDelta = addRight rightDelta this
        member inline this.SetIdx left right = setIdx left right this