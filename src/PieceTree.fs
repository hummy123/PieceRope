namespace HumzApps.TextDocument

open System.Text

type PieceTreeHeight = int

type PieceTreeLeftSize = int

type PieceTreeLeftLines = int

type PieceTreeRightSize = int

type PieceTreeRightLines = int

type PieceStart = int

type PieceLength = int

type PieceLines = int array

/// A PieceTree is a type storing pointers to the PieceBuffer that, when both are taken together, rebuilds the text the user expects.
type PieceTree =
  | PE
(* 
    Using a very long tuple because I want to allocate fewer objects and it does not make code much harder to parse.
    Usually these are only accessed directly in the AVL-Tree specific functions. 
    I do not generally encourage extremely long tuples like this.
*)
  | PT of PieceTreeHeight       * 
          PieceTree             *
          PieceTreeLeftLines    *
          PieceTreeLeftSize     *
          PieceStart            *
          PieceLength           *
          PieceLines            *
          PieceTreeRightSize    *
          PieceTreeRightLines   *
          PieceTree

/// A PieceLine stores the content of a line and the index the line starts at.
type PieceLine = {
  Content: string;
  StartIndex: int;
}

[<RequireQualifiedAccess>]
module internal PieceTree =
  let empty = PE

  let inline private topLevelCont x = x

  (* Folds over pieces in a PieceTree in order. Useful for other functions such as saving, serialisation or retrieving text. *)
  let inline foldPieces folder initialState tree =
    let rec fold state node cont =
      match node with
      | PE -> 
          state |> cont
      | PT(_, l, _, _, pcStart, pcLength, pcLines, _, _, r) ->
          fold state l (fun state ->
            let state = folder state pcStart pcLength pcLines
            fold state r (fun x -> x |> cont)
          )
    fold initialState tree topLevelCont

  (* Maximum height difference tolerated between two sibling subtree nodes. *)
  [<Literal>]
  let private TOLERANCE = 2

  let inline private nLength node =
    match node with
    | PE -> 0
    | PT(_, _, _, _, _, len, _, _, _, _) -> len

  let inline private nLines node =
    match node with
    | PE -> 0
    | PT(_, _, _, _, _, _, lines, _, _, _) -> lines.Length

  let inline size tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, lm, _, len, _, rm, _, _) -> len + lm + rm

  let inline lines tree =
    match tree with
    | PE -> 0
    | PT(_, _, ll, _, _, _, lines, _, rl, _) -> ll + rl + lines.Length

  let inline private sizeLeft tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, lm, _, _, _, _, _, _) -> lm

  let inline private sizeRight tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, _, _, _, _, rm, _, _) -> rm

  let inline private linesLeft tree =
    match tree with
    | PE -> 0
    | PT(_, _, ll, _, _, _, _, _, _, _) -> ll

  let inline private linesRight tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, _, _, _, _, _, rl, _) -> rl

(* AVL Tree-specific functions. 
 * Adapted from verified proof at https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/document.pdf . *)
  let inline ht tree =
    match tree with
    | PE -> 0
    | PT(h, _, _, _, _, _, _, _, _, _) -> h

  let inline private mk l pcStart pcLength pcLines r =
    let h = (if ht l > ht r then ht l else ht r) + 1
    PT(h, l, lines l, size l, pcStart, pcLength, pcLines, size r, lines r, r)

  let inline private balL ab xStart xLength xLines c =
    if ht ab = ht c + TOLERANCE then
      match ab with
      | PT(_, a, _, _, yStart, yLength, yLines, _, _, b) ->
          if ht a >= ht b then
            mk a yStart yLength yLines (mk b xStart xLength xLines c) 
          else
            match b with
            | PT(_, b1, _, _, bxStart, bxLength, bxLines, _, _, b2) ->
                mk (mk a yStart yLength yLines b1) bxStart bxLength bxLines (mk b2 xStart xLength xLines c)
            | _ -> mk ab xStart xLength xLines c
      | _ -> mk ab xStart xLength xLines c
    else
      mk ab xStart xLength xLines c

  let inline private balR a xStart xLength xLines bc =
    if ht bc = ht a + TOLERANCE then
      match bc with
      | PT(_, b, _, _, yStart, yLength, yLines, _, _, c) ->
          if ht b <= ht c then
            mk (mk a xStart xLength xLines b) yStart yLength yLines c
          else
            match b with
            | PT(_, b1, _, _, bxStart, bxLength, bxLines, _, _, b2) ->
                mk (mk a xStart xLength xLines b1) bxStart bxLength bxLines (mk b2 yStart yLength yLines c)
            | _ -> mk a xStart xLength xLines bc
      | _ -> mk a xStart xLength xLines bc
    else
      mk a xStart xLength xLines bc

  let rec private splitMax node cont =
    match node with
    | PT(_, l, _, _, pcStart, pcLength, pcLines, _, _, r) ->
        if r = PE then
          (l, pcStart, pcLength, pcLines) |> cont
        else
          splitMax r (fun (r', r'Start, r'Length, r'Lines) -> (balL l pcStart pcLength pcLines r', r'Start, r'Length, r'Lines) |> cont)
    | PE -> failwith "unexpected splitMax case"

(* Logic for handling piece data. *)
  let inline private splitLines rStart lines =
    match Array.tryFindIndex(fun x -> x >= rStart) lines with
    | Some splitPoint ->
        let arrLeft = Array.sub lines 0 splitPoint
        let arrRight = Array.sub lines splitPoint (lines.Length - splitPoint)
        arrLeft, arrRight
    | None ->
      lines, [||]

  let inline private deleteLinesInRange p1Length p2Start lines =
    let p1Lines = ResizeArray()
    let p2Lines = ResizeArray()
    for i in lines do
        if i < p1Length
        then p1Lines.Add i
        elif i >= p2Start
        then p2Lines.Add i
    p1Lines.ToArray(), p2Lines.ToArray()

  let inline private deleteInRange curIndex start finish pieceStart pieceLength pieceLines =
    (* p1 retains metadata and p2 is leaf *)
    let finishDifference = finish - curIndex
    let p1Length = start - curIndex
    let p2Start = finishDifference + pieceStart
    let (p1Lines, p2Lines) = deleteLinesInRange (p1Length + pieceStart) p2Start pieceLines
    let p2Length = pieceLength - finishDifference
    (p1Length, p1Lines, p2Start, p2Length, p2Lines)

  let inline private deleteAtStart curIndex finish pieceStart pieceLength pieceLines =
    let difference = finish - curIndex
    let newStart = pieceStart + difference
    let newLength = pieceLength - difference
    let newLines = Array.skipWhile (fun x -> x < difference) pieceLines
    (newStart, newLength, newLines)

  let inline private deleteAtEnd curIndex start pieceLines =
    let length = start - curIndex
    let lines = Array.takeWhile (fun x -> x <= length) pieceLines
    (length, lines)

  let inline private pieceText pieceStart pieceLength buffer =
    PieceBuffer.substring pieceStart pieceLength buffer

  let inline private textInRange curIndex start finish pieceStart buffer =
    let textStart = start - curIndex + pieceStart
    let textLength = finish - curIndex + pieceStart - textStart
    PieceBuffer.substring textStart textLength buffer

  let inline private textAtStart curIndex finish pieceStart buffer =
    let textLength = finish - curIndex
    PieceBuffer.substring pieceStart textLength buffer

  let inline private textAtEnd curIndex start pieceStart pieceLength buffer =
    let textStart = start - curIndex + pieceStart
    let textLength = pieceStart + pieceLength - textStart
    PieceBuffer.substring textStart textLength buffer

  /// Returns a substring at the provided start and length.
  let inline private atStartAndLength start length buffer =
    PieceBuffer.substring start length buffer

(* Core PieceTree logic. *)
  let inline private isConsecutive existingStart existingLength insStart =
    existingStart + existingLength = insStart

  /// Inserts a piece at the start of the tree.
  let inline prepend pcStart pcLength pcLines tree =
    let rec pre node cont =
      match node with
      | PE -> 
          mk PE pcStart pcLength pcLines PE |> cont
      | PT(_, l, _, _, nodeStart, nodeLength, nodeLines, _, _, r) ->
          pre l (fun l' -> balL l' nodeStart nodeLength nodeLines r |> cont)
    pre tree topLevelCont

  /// Inserts a piece at the end of the tree. Will not merge two consecutive pieces.
  let inline private insMax pcStart pcLength pcLines tree =
    let rec max node cont =
      match node with
      | PE ->
          mk PE pcStart pcLength pcLines PE |> cont
      | PT(_, l, _, _, nodeStart, nodeLength, nodeLines, _, _, r) ->
          max r (fun r' -> balR l nodeStart nodeLength nodeLines r' |> cont)
    max tree topLevelCont

  /// Appends a piece to the end of the tree. Will merge with the currently-last piece if possible.
  let inline append pcStart pcLength pcLines tree =
    let rec app node cont =
      match node with
      | PE ->
          mk PE pcStart pcLength pcLines PE |> cont
      | PT(_, l, _, _, nodeStart, nodeLength, nodeLines, _, _, PE) when isConsecutive nodeStart nodeLength pcStart ->
          mk l nodeStart (nodeLength + pcLength) (Array.append nodeLines pcLines) PE |> cont
      | PT(_, l, _, _, nodeStart, nodeLength, nodeLines, _, _, r) ->
          app r (fun r' -> balR l nodeStart nodeLength nodeLines r' |> cont)
    app tree topLevelCont

  /// Inserts a piece into a tree at the specified index. Will merge if this piece is consecutive with another.
  let insert insIndex pcStart pcLength pcLines tree =
    let rec ins curIndex node cont =
      match node with
      | PE -> 
          mk PE pcStart pcLength pcLines PE |> cont
      | PT(_, l, _, lidx, nodeStart, nodeLength, nodeLines, ridx, _, r) ->
          let nodeEndIndex = curIndex + nodeLength
          if insIndex < curIndex then
            let nextIndex = curIndex - nLength l - sizeRight l 
            ins nextIndex l (fun l' -> balL l' nodeStart nodeLength nodeLines r |> cont)
          elif insIndex > nodeEndIndex then
            let nextIndex = nodeEndIndex + sizeLeft r
            ins nextIndex r (fun r' -> balR l nodeStart nodeLength nodeLines r' |> cont)
          elif insIndex = curIndex then
            let l' = insMax pcStart pcLength pcLines l
            balL l' nodeStart nodeLength nodeLines r |> cont
          elif insIndex = nodeEndIndex then
            if isConsecutive nodeStart nodeLength pcStart then
              mk l nodeStart (nodeLength + pcLength) (Array.append nodeLines pcLines) r |> cont
            else
              let r' = prepend pcStart pcLength pcLines r
              balR l nodeStart nodeLength nodeLines r' |> cont
          else
            let difference = insIndex - curIndex
            let rStart = nodeStart + difference
            let rLength = nodeLength - difference
            let (leftLines, rightLines) = splitLines rStart nodeLines
            let l' = insMax nodeStart difference leftLines l
            let r' = prepend rStart rLength rightLines r
            mk l' pcStart pcLength pcLines r' |> cont
    ins (sizeLeft tree) tree topLevelCont
            

(* Repeated if-statements used both for delete and substring. *)
  let inline private nodeInRange start curIndex finish nodeEndIndex =
    start <= curIndex && finish >= nodeEndIndex

  let inline private startOfNodeInRange start curIndex finish nodeEndIndex =
    start <= curIndex && finish < nodeEndIndex && curIndex < finish

  let inline private endOfNodeInRange start curIndex finish nodeEndIndex =
    start > curIndex && finish >= nodeEndIndex && start <= nodeEndIndex

  let inline private middleOfNodeInSubstringRange start curIndex finish nodeEndIndex =
    start >= curIndex && finish <= nodeEndIndex

  let delete start length tree =
    let finish = start + length
    let rec del curIndex node cont =
      match node with
      | PE -> 
          PE |> cont
      | PT(_, l, _, lidx, nodeStart, nodeLength, nodeLines, ridx, _, r) ->
          let nodeEndIndex = curIndex + nodeLength
          (* Whole |node| is range. *)
          if nodeInRange start curIndex finish nodeEndIndex then
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            let recurseRightIndex = nodeEndIndex + sizeLeft r

            del recurseLeftIndex l (fun l' ->
              del recurseRightIndex r (fun r' ->
                if l' = PE then
                  r' |> cont
                else
                  let (newLeft, newStart, newLength, newLines) = splitMax l' topLevelCont
                  balR newLeft newStart newLength newLines r' |> cont
              )
            )
          (* Start of |no|de in range which means start of node but end of range. *)
          elif startOfNodeInRange start curIndex finish nodeEndIndex then
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            del recurseLeftIndex l (fun l' ->
              let (newStart, newLength, newLines) = deleteAtStart curIndex finish nodeStart nodeLength nodeLines
              balR l' newStart newLength newLines r |> cont
            )
          (* End of no|de| in range which means end of node but start of range. *)
          elif endOfNodeInRange start curIndex finish nodeEndIndex then
            let recurseRightIndex = nodeEndIndex + sizeLeft r
            del recurseRightIndex r (fun r' ->
              let (length, lines) = deleteAtEnd curIndex start nodeLines
              balL l nodeStart length lines r' |> cont
            )
          (* Range is in middle of n|od|e which means we don't need to recurse further. *)
          elif middleOfNodeInSubstringRange start curIndex finish nodeEndIndex then
            let (p1Length, p1Lines, p2Start, p2Length, p2Lines) = 
              deleteInRange curIndex start finish nodeStart nodeLength nodeLines
            let r' = prepend p2Start p2Length p2Lines r
            balR l nodeStart p1Length p1Lines r' |> cont
          (* Range is to left so recurse leftwards. *)
          elif start < curIndex then
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            del recurseLeftIndex l (fun l' -> balR l' nodeStart nodeLength nodeLines r |> cont)
          (* Range is to right so recurse rightwards. *)
          else
            let recurseRightIndex = nodeEndIndex + sizeLeft r
            del recurseRightIndex r (fun r' -> balL l nodeStart nodeLength nodeLines r' |> cont)
    del (sizeLeft tree) tree topLevelCont

  let substring (start: int) (length: int) tree buffer =
    let finish = start + length
    let sb = StringBuilder(length)
    let rec sub curIndex node cont =
      match node with
      | PE -> () |> cont
      | PT(_, l, _, lidx, nodeStart, nodeLength, nodeLines, ridx, _, r) ->
          let nodeEndIndex = curIndex + nodeLength
          if endOfNodeInRange start curIndex finish nodeEndIndex then
            let recurseRightIndex = nodeEndIndex + sizeLeft r
            sb.Append (textAtEnd curIndex start nodeStart nodeLength buffer) |> ignore
            sub recurseRightIndex r (fun x -> x |> cont)
          elif nodeInRange start curIndex finish nodeEndIndex then
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            let recurseRightIndex = nodeEndIndex + sizeLeft r
            sub recurseLeftIndex l (fun _ ->
              sb.Append (pieceText nodeStart nodeLength buffer) |> ignore
              sub recurseRightIndex r (fun x -> x |> cont)
            )
          elif startOfNodeInRange start curIndex finish nodeEndIndex then
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            sub recurseLeftIndex l (fun _ -> 
              let text = textAtStart curIndex finish nodeStart buffer
              cont(sb.Append text |> ignore)
            )
          elif middleOfNodeInSubstringRange start curIndex finish nodeEndIndex then
            sb.Append (textInRange curIndex start finish nodeStart buffer) |> ignore
            cont()
          elif start < curIndex then
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            sub recurseLeftIndex l (fun x -> x |> cont)
          else
            let recurseRightIndex = nodeEndIndex + sizeLeft r
            sub recurseRightIndex r (fun x -> x |> cont)
    sub (sizeLeft tree) tree topLevelCont
    sb.ToString()

(* Delete/substring if-statements adapted to work with lines. *)
  let inline private nodeInLine nodeStartLine searchLine nodeEndLine =
    nodeStartLine = searchLine && searchLine = nodeEndLine

  let inline private endOfLineInNode nodeStartLine searchLine nodeEndLine =
    nodeStartLine = searchLine && searchLine < nodeEndLine

  let inline private startOfLineInNode nodeStartLine searchLine nodeEndLine =
    nodeStartLine < searchLine && searchLine = nodeEndLine

  let inline private lineWithinNode nodeStartLine searchLine nodeEndLine =
    nodeStartLine < searchLine && nodeEndLine > searchLine

  let getLineAndLineStartIndex line tree buffer =
    let sb = StringBuilder()
    let rec get curLine curIndex node cont =
      match node with
      | PE -> None |> cont
      | PT(_, l, _, lidx, nodeStart, nodeLength, nodeLines, ridx, _, r) ->
          let nodeEndLine = curLine + nodeLines.Length

          if startOfLineInNode curLine line nodeEndLine then
            let lineStart = nodeLines[nodeLines.Length - 1] + 1
            let length = nodeLength - lineStart + nodeStart
            sb.Append (atStartAndLength lineStart length buffer) |> ignore
            
            let lineStartIndex = Some(curIndex + nodeLength - lineStart)
            let recurseRightLine = nodeEndLine + linesLeft r
            let recurseRightIndex = curIndex + nodeLength + sizeLeft r
            get recurseRightLine recurseRightIndex r (fun _ -> lineStartIndex |> cont)

          elif nodeInLine curLine line nodeEndLine then
            let recurseLeftLine = curLine - nLines l - linesRight l
            let recurseRightLine = nodeEndLine + linesLeft r
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            let recurseRightIndex = curIndex + nodeLength + sizeLeft r
            get recurseLeftLine recurseLeftIndex l (fun lidx ->
              sb.Append (pieceText nodeStart nodeLength buffer) |> ignore
              get recurseRightLine recurseRightIndex r (fun _ ->
                match lidx with
                | Some _ -> lidx |> cont
                | None -> (Some curIndex) |> cont
              )
            )

          elif endOfLineInNode curLine line nodeEndLine then
            let length = nodeLines[0] + 1 - nodeStart
            let recurseLeftLine = curLine - nLines l - linesRight l
            let recurseRightLine = nodeEndLine + linesLeft r
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            let recurseRightIndex = curIndex + nodeLength + sizeLeft r
            get recurseLeftLine recurseLeftIndex l (fun lidx ->
              sb.Append (atStartAndLength nodeStart length buffer) |> ignore
              match lidx with
                | Some _ -> lidx |> cont
                | None -> (Some curIndex) |> cont
            )

          elif lineWithinNode curLine line nodeEndLine then
            let lineDifference = line - curLine
            let lineStart = nodeLines[lineDifference - 1] + 1
            let lineLength = nodeLines[lineDifference] - lineStart + 1
            sb.Append (atStartAndLength lineStart lineLength buffer) |> ignore
            let lineStartIndex = Some((lineStart - nodeStart) - curIndex)
            lineStartIndex |> cont

          elif line < curLine then
            let recurseLeftLine = curLine - nLines l - linesRight l
            let recurseLeftIndex = curIndex - nLength l - sizeRight l
            get recurseLeftLine recurseLeftIndex l (fun x -> x |> cont)

          else
            let recurseRightLine = nodeEndLine + linesLeft r
            let recurseRightIndex = curIndex + nodeLength + sizeLeft r
            get recurseRightLine recurseRightIndex r (fun x -> x |> cont)

    match get (linesLeft tree) (sizeLeft tree) tree topLevelCont with
    | Some idx ->
        { Content = sb.ToString(); StartIndex = idx; }
    | None ->
        { Content = sb.ToString(); StartIndex = 0; }

  let getLine line tree buffer = 
    let content = getLineAndLineStartIndex line tree buffer
    content.Content

  let getText tree buffer =
    let sb = StringBuilder(size tree)
    foldPieces (fun _ start length _ -> 
      sb.Append (atStartAndLength start length buffer) |> ignore
    ) () tree
    sb.ToString()

