namespace HumzApps.TextDocument

open System.Text

type PieceBufferHeight = int

type PieceBufferLeftSize = int

type PieceBufferRightSize = int

/// A PieceBuffer represents a Buffer which stores text in the abstract Piece Table data structure.
/// This implementation ignores the traditional distinction between an "original" and "append-only" buffer because all the text is stored in memory.
/// Ignoring this distinction lets us reduce memory consumption and provide a more convenient API.
type PieceBuffer =
  | BE
  | BT of PieceBufferHeight * PieceBuffer * PieceBufferLeftSize * PieceString * PieceBufferRightSize * PieceBuffer

[<RequireQualifiedAccess>]
module internal PieceBuffer =
  (* If node's string length + append string length are greater than this, create new node when appending. Else concat strings. *)
  [<Literal>]
  let private MAX_CONCAT_LENGTH = 1024

  (* Maximum height difference tolerated between two sibling subtree nodes. *)
  [<Literal>]
  let private TOLERANCE = 2

  (* This seemed to make continuation passing style faster when used as the initial call to a recursive function. *)
  let inline private topLevelCont x = x

  (* Folds over the strings in a PieceBuffer in order. Useful for serialisation. *)
  let inline internal foldStrings folder initialState buffer =
    let rec fold state node cont =
      match node with
      | BE -> 
          state |> cont
      | BT(_, l, _, v, _, r) ->
          fold state l (fun state ->
            let state = folder v
            fold state r (fun state -> state |> cont)
          )
    fold initialState buffer topLevelCont

  (* Retrieve PieceBuffer metadata. *)
  let inline size tree =
    match tree with
    | BE -> 0
    | BT(_, _, lm, v, rm, _) -> lm + v.Length + rm

  let inline private stringLength tree =
    match tree with
    | BE -> 0
    | BT(_, _, _, v, _, _) -> v.Length

  let inline private sizeLeft tree =
    match tree with
    | BE -> 0
    | BT(_, _, lm, _, _, _) -> lm

  let inline private sizeRight tree =
    match tree with
    | BE -> 0
    | BT(_, _, _, _, rm, _) -> rm

  (* AVL Tree-specific functions. 
   * Adapted from verified proof at https://isabelle.in.tum.de/library/HOL/HOL-Data_Structures/document.pdf . *)
  let inline private ht node =
    match node with
    | BE -> 0
    | BT(h, _, _, _, _, _) -> h

  let inline private mk l a r =
    let hl = ht l
    let hr = ht r
    let h = (if hl > hr then hl else hr) + 1
    BT(h, l, size l, a, size r, r)

  let inline private balR a x bc =
    if ht bc = ht a + TOLERANCE then
      match bc with
      | BT(_, b, _, y, _, c) ->
        if ht b <= ht c then
          mk (mk a x b) y c
        else
          match b with
          | BT(_, b1, _, bx, _, b2) -> mk (mk a x b1) bx (mk b2 y c)
          | x -> x
      | x -> x
    else
      mk a x bc

  /// An empty instance of the PieceBuffer type.
  let empty = BE

  /// Appends a string to a PieceBuffer.
  let inline append (string: string) charBreaks buffer =
    let rec app node cont =
      match node with
      | BE -> 
          mk BE (U(string, charBreaks)) BE |> cont
      | BT(h, l, lm, v, rm, r) when v.Length + string.Length <= MAX_CONCAT_LENGTH ->
          BT(h, l, lm, v.Concat string charBreaks, rm, r) |> cont
      | BT(_, l, _, v, _, r) ->
          app r (fun r' -> balR l (v.Compress()) r' |> cont)
    app buffer topLevelCont

  (* Below functions provide a descriptive name for the substring cases we need to check for,
   * making code that uses them easier to read. *)
  let inline private nodeInSubstringRange start curIndex finish nodeEndIndex = 
    start <= curIndex && finish >= nodeEndIndex

  let inline private startOfNodeInSubstringRange start curIndex finish nodeEndIndex =
    start <= curIndex && finish < nodeEndIndex && curIndex < finish

  let inline private endOfNodeInSubstringRange start curIndex finish nodeEndIndex =
    start > curIndex && finish >= nodeEndIndex && start <= nodeEndIndex

  let inline private substringRangeInMiddleOfNode start curIndex finish nodeEndIndex =
    start >= curIndex && finish <= nodeEndIndex

  /// Returns a substring from a PieceBuffer.
  let inline substring (start: int) (length: int) buffer =
    let finish = start + length
    let sb = StringBuilder(length)
    let rec sub curIndex node cont =
      match node with
      | BE -> () |> cont
      | BT(_, l, lm, v, rm, r) -> 
          let nodeEndIndex = curIndex + v.Length

          if nodeInSubstringRange start curIndex finish nodeEndIndex then
            sub (curIndex - stringLength l) l (fun _ -> 
              sb.Append v |> ignore
              sub (nodeEndIndex + sizeLeft r) r (fun x -> x |> cont)
            )
          elif startOfNodeInSubstringRange start curIndex finish nodeEndIndex then
            sub (curIndex - stringLength l - sizeRight l) l (fun _ -> 
              let length = finish - curIndex
              cont(sb.Append (v.Substring(0, length)) |> ignore)
            )
          elif endOfNodeInSubstringRange start curIndex finish nodeEndIndex then
            let strStart = start - curIndex
            let len = v.Length - strStart
            sb.Append(v.Substring(strStart, len)) |> ignore
            sub (curIndex + v.Length + sizeLeft r) r (fun x -> x |> cont)
          elif substringRangeInMiddleOfNode start curIndex finish nodeEndIndex then
            let strStart = start - curIndex
            sb.Append(v.Substring(strStart, length)) |> ignore
          elif start < curIndex then
            sub (curIndex - stringLength l - sizeRight l) l (fun x -> x |> cont)
          else
            sub (nodeEndIndex + sizeLeft r) r (fun x -> x |> cont)

    sub (sizeLeft buffer) buffer topLevelCont |> ignore
    sb.ToString()

