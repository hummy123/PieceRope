namespace Buffer

open Types

module internal Node =
    let inline create string = 
        { String = string; LeftIdx = 0; RightIdx = 0; }

    let inline addLeft leftDelta node =
        { node with LeftIdx = node.LeftIdx + leftDelta }

    let inline addRight rightDelta node =
        { node with RightIdx = node.RightIdx + rightDelta }

    let inline setIdx left right node =
        { String = node.String; LeftIdx = left; RightIdx = right; }

    type BufferNode with
        member inline this.AddLeft delta = addLeft delta this
        member inline this.AddRight delta = addRight delta this
        member inline this.SetIdx left right = setIdx left right this
