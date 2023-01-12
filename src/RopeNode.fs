namespace AppendRope

open AppendRopeTypes

module internal RopeNode =
    let inline create string = 
        { String = string; LeftIdx = 0; RightIdx = 0; }

    let inline concat string node =
        node.String <- node.String + string

    let inline addLeft leftDelta node =
        node.LeftIdx <- node.LeftIdx + leftDelta

    let inline addRight rightDelta node =
        node.RightIdx <- node.RightIdx + rightDelta

    let inline setIdx left right node =
        node.LeftIdx <- left
        node.RightIdx <- right

    type RopeNode with
        member inline this.Concat string = concat string this
        member inline this.AddLeft delta = addLeft delta this
        member inline this.AddRight delta = addRight delta this
        member inline this.SetIdx left right = setIdx left right this
