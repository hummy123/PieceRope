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
  (* Using a very long tuple because I want to allocate fewer objects and it does nt make code much harder to parse.
     Usually these are only accessed directly in the AVL-Tree specific functions. *)
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

module internal PieceTree =
  (* Maximum height difference tolerated between two sibling subtree nodes. *)
  [<Literal>]
  let private TOLERANCE = 2

  let inline nLength node =
    match node with
    | PE -> 0
    | PT(_, _, _, _, _, len, _, _, _, _) -> len

  let inline nLines node =
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

  let inline sizeLeft tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, lm, _, _, _, _, _, _) -> lm

  let inline sizeRight tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, _, _, _, _, rm, _, _) -> rm

  let inline linesLeft tree =
    match tree with
    | PE -> 0
    | PT(_, _, ll, _, _, _, _, _, _, _) -> ll

  let inline linesRight tree =
    match tree with
    | PE -> 0
    | PT(_, _, _, _, _, _, _, _, rl, _) -> rl

  (* AVL Tree-specific functions. 
   * Adapted from verified proof at https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/document.pdf . *)
  let inline ht tree =
    match tree with
    | PE -> 0
    | PT(h, _, _, _, _, _, _, _, _, _) -> h

  let inline mk l pcStart pcLength pcLines r =
    let h = (if ht l > ht r then ht l else ht r) + 1
    PT(h, l, lines l, size l, pcStart, pcLength, pcLines, size r, lines r, r)

  let inline balL ab xStart xLength xLines c =
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

  let inline balR a xStart xLength xLines bc =
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

  let rec splitMax node cont =
    match node with
    | PT(_, l, _, _, pcStart, pcLength, pcLines, _, _, r) ->
        if r = PE then
          (l, pcStart, pcLength, pcLines) |> cont
        else
          splitMax r (fun (r', r'Start, r'Length, r'Lines) -> (balL l pcStart pcLength pcLines r', r'Start, r'Length, r'Lines) |> cont)
    | PE -> failwith "unexpected splitMax case"
