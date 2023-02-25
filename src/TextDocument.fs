namespace rec HumzApps.TextBuffer
(*
  Recursive namespace is only for attaching methods to TextDocument type
  for easier OOP-style consumption for C# users.
  There are no other circular dependencies.
 *)

open System.Globalization

/// A TextDocument is an immutable data structure for manupilating text efficiently.
type TextDocument = {
  Buffer: PieceBuffer;
  Pieces: PieceTree;
}
  with
  member inline this.Length  = PieceTree.size this.Pieces
  member inline this.Lines   = PieceTree.lines this.Pieces

  member inline this.Insert index string  = TextDocument.insert index string this
  member inline this.Delete start length  = TextDocument.delete start length this
  member inline this.Prepend string       = TextDocument.prepend string this
  member inline this.Append string        = TextDocument.append string this

  member inline this.Substring start length = TextDocument.substring start length this
  member inline this.GetLine line           = TextDocument.getLine line this
  member inline this.Text()                 = TextDocument.text this

module TextDocument =
  let empty = { Buffer = PieceBuffer.empty; Pieces = PieceTree.empty }

  (* Preprocess string to generate an array of line breaks and a lookup table for grapheme clusters. *)
  let inline private preprocessString (string: string) pcStart =
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

  let insert index (string: string) document =
    let pcStart = PieceBuffer.size document.Buffer
    let (lineBreaks, charBreaks) = preprocessString string pcStart
    let buffer = PieceBuffer.append string document.Buffer
    let pieces = PieceTree.insert index pcStart charBreaks.Length lineBreaks document.Pieces
    { Buffer = buffer; Pieces = pieces; }

  let prepend (string: string) document =
    let pcStart = PieceBuffer.size document.Buffer
    let (lineBreaks, charBreaks) = preprocessString string pcStart
    let buffer = PieceBuffer.append string document.Buffer
    let pieces = PieceTree.prepend pcStart charBreaks.Length lineBreaks document.Pieces
    { Buffer = buffer; Pieces = pieces; }
    
  let append (string: string) document =
    let pcStart = PieceBuffer.size document.Buffer
    let (lineBreaks, charBreaks) = preprocessString string pcStart
    let buffer = PieceBuffer.append string document.Buffer
    let pieces = PieceTree.append pcStart charBreaks.Length lineBreaks document.Pieces
    { Buffer = buffer; Pieces = pieces; }

  let delete start length document =
    let pieces = PieceTree.delete start length document.Pieces
    { document with Pieces = pieces; }

  let create string = append string empty

  let substring start length document =
    PieceTree.substring start length document.Pieces document.Buffer

  let getLine line document =
    PieceTree.getLine line document.Pieces document.Buffer

  let text document =
    PieceTree.getText document.Pieces document.Buffer

