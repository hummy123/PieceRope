namespace HumzApps.TextBuffer

type AvlHeight = int

type LeftSize = int

type LeftLines = int

type RightSize = int

type RightLines = int

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
  | PT of AvlHeight     * 
          PieceTree     *
          LeftLines     *
          LeftSize      *
          PieceStart    *
          PieceLength   *
          PieceLines    *
          RightSize     *
          RightLines    *
          PieceTree

[<RequireQualifiedAccess>]
module internal PieceTree =
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

  let inline private size tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, lm, _, len, _, rm, _, _) -> len + lm + rm

  let inline private lines tree =
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
  let inline private ht tree =
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

  let inline deleteInRange curIndex start finish pieceStart pieceLength pieceLines =
    (* p1 retains metadata and p2 is leaf *)
    let finishDifference = finish - curIndex
    let p1Length = start - curIndex
    let p2Start = finishDifference + pieceStart
    let (p1Lines, p2Lines) = deleteLinesInRange (p1Length + pieceStart) p2Start pieceLines
    let p2Length = pieceLength - finishDifference
    (p1Length, p1Lines, p2Start, p2Length, p2Lines)

  let inline deleteAtStart curIndex finish pieceStart pieceLength pieceLines =
    let difference = finish - curIndex
    let newStart = pieceStart + difference
    let newLength = pieceLength - difference
    let newLines = Array.skipWhile (fun x -> x < difference) pieceLines
    (newStart, newLength, newLines)

  let inline deleteAtEnd curIndex start pieceLines =
    let length = start - curIndex
    let lines = Array.takeWhile (fun x -> x <= length) pieceLines
    (length, lines)

  let inline text pieceStart pieceLength buffer =
    PieceBuffer.substring pieceStart pieceLength buffer

  let inline textInRange curIndex start finish pieceStart buffer =
    let textStart = start - curIndex + pieceStart
    let textLength = finish - curIndex + pieceStart - textStart
    PieceBuffer.substring textStart textLength buffer

  let inline textAtStart curIndex finish pieceStart buffer =
    let textLength = finish - curIndex
    PieceBuffer.substring pieceStart textLength buffer

  let inline textAtEnd curIndex start pieceStart pieceLength buffer =
    let textStart = start - curIndex + pieceStart
    let textLength = pieceStart + pieceLength - textStart
    PieceBuffer.substring textStart textLength buffer

  /// Returns a substring at the provided start and length.
  let inline atStartAndLength start length buffer =
    PieceBuffer.substring start length buffer
