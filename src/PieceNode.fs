namespace PieceTree

open PieceTree.Types

module Node =
    let inline create start length lines = 
        { Start = start; 
        Length = length; 
        LeftIdx = 0; 
        RightIdx = 0;
        LeftLn = 0;
        RightLn = 0; 
        Lines = lines; }

    let inline addLeft idxDelta lnDelta node =
        { node with LeftIdx = node.LeftIdx + idxDelta; LeftLn = node.LeftLn + lnDelta }

    let inline addRight idxDelta lnDelta node =
        { node with RightIdx = node.RightIdx + idxDelta; RightLn = node.RightLn + lnDelta }

    let inline setIdx left right node =
        { node with LeftIdx = left; RightIdx = right; }

    let inline setData leftSize leftLns rightSize rightLns node =
        { node with 
            LeftIdx = leftSize;
            LeftLn = leftLns; 
            RightIdx = rightSize;
            RightLn = rightLns; }

    type PieceNode with
        member inline this.AddLeft idxDelta lnDelta = addLeft idxDelta lnDelta this
        member inline this.AddRight idxDelta lnDelta = addRight idxDelta lnDelta this
        member inline this.SetIdx left right = setIdx left right this
        member inline this.SetData (leftSize, leftLns) (rightSize, rightLns) = 
            setData leftSize leftLns rightSize rightLns this