namespace PieceTree

open PieceTree.Types

module Node =
    let inline create start length = 
        let data = [|start; length; 0; 0 |]
        { Data = data }

    let inline createWithMetadata start length left right =
        let data = [|start; length; left; right |]
        { Data = data }

    let inline addLeft leftDelta node =
        let data = [| node.Data[0]; node.Data[1]; node.Data[2] + leftDelta; node.Data[3] |]
        { Data = data }

    let inline addRight rightDelta node =
        let data = [| node.Data[0]; node.Data[1]; node.Data[2]; node.Data[3] + rightDelta |]
        { Data = data }

    let inline setLeftIdx left node =
        let data = [|node.Data[0]; node.Data[1]; left; node.Data[3] |]
        { Data = data }

    let inline setRightIdx right node =
        let data = [|node.Data[0]; node.Data[1]; node.Data[2]; right |]
        { Data = data }

    let inline setIdx left right node =
        let data = [|node.Data[0]; node.Data[1]; left; right |]
        { Data = data }

    let inline setStart start node =
        let data = [|start; node.Data[1]; node.Data[2]; node.Data[3];|]
        { Data = data }

    let inline setLength length node =
        let data = [|node.Data[0]; length; node.Data[2]; node.Data[3];|]
        { Data = data }

    let inline setStartAndLength start length node =
        let data = [|start; length; node.Data[2]; node.Data[3];|]
        { Data = data }

    type PieceNode with
        member inline this.AddLeft leftDelta = addLeft leftDelta this
        member inline this.AddRight rightDelta = addRight rightDelta this
        member inline this.SetLeftIdx left  = setLeftIdx left this
        member inline this.SetRightIdx right  = setRightIdx right this
        member inline this.SetIdx left right = setIdx left right this
        member inline this.SetStart start = setStart start this
        member inline this.SetLength length = setLength length this
        member inline this.SetStartAndLength start length = setStartAndLength start length this
