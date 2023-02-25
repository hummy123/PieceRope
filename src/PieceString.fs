namespace HumzApps.TextDocument

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

