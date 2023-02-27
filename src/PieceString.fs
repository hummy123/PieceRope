namespace HumzApps.TextDocument

open System.Globalization

type PieceString =
  | S of string               (* Normal UTF-16 string. *)
  | U of string * int array   (* UTF-16 string with array look-up table for grapheme clusters. *)
  with 
  /// If the lookup table is the same length as the string, discard it to save memory.
  member inline this.Compress() =
    match this with
    | S _ -> this
    | U(str, arr) ->
        if arr.Length = str.Length then 
          S(str)
        else
          this

  /// Retrieves a substring, not splitting on any grapheme clusters.
  member inline this.Substring(start, length) =
    match this with
    | S s -> s.Substring(start, length)
    | U(str, arr) ->
        if start = arr.Length then
          let strStart = arr[start - 1] + 1
          str.Substring(strStart, str.Length - strStart)
        else
          let finish = start + length
          let strStart = arr[start]
          if finish = arr.Length then
            str.Substring(strStart, str.Length - strStart)
          else
            let length = arr[finish] - strStart
            str.Substring(strStart, length)

  /// Retrieves the length in terms of grapheme clusters.
  member inline this.Length =
    match this with
    | S s -> s.Length
    | U(_, arr) -> arr.Length

  member inline this.Concat string arr =
    match this with
    (* All strings are in U case before we compress, and we only compress after we're done concatenating. *)
    | S _ -> failwith "unexpected PieceString.Concat"
    | U(oldStr, oldArr) ->
        let arr = 
          Array.map (fun x -> x + oldStr.Length) arr
          |> Array.append oldArr
        U(oldStr + string, arr)

  override this.ToString() =
    match this with
    | S s -> s
    | U(s, _) -> s

[<RequireQualifiedAccess>]
module internal PieceString =  
  /// Preprocess string to generate an array of line breaks and a lookup table for grapheme clusters.
  let inline preprocessString (string: string) pcStart =
    let enumerator = StringInfo.GetTextElementEnumerator(string)
    let lineBreaks = ResizeArray() 
    let charBreaks = ResizeArray(string.Length) (* Unicode line / character breaks. *)

    let mutable graphemePos = 0 (* Tracking character count in terms of grapheme clusters (not UTF-16). *)
    let mutable cur = "" (* Current TextElement. *)

    while enumerator.MoveNext() do
      cur <- enumerator.GetTextElement()
      if cur.Contains("\n") || cur.Contains("\r") then 
        lineBreaks.Add(graphemePos + pcStart)
      charBreaks.Add enumerator.ElementIndex
      graphemePos <- graphemePos + 1

    lineBreaks.ToArray(), charBreaks.ToArray()



