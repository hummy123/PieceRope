namespace rec HumzApps.TextDocument
(*
  Recursive namespace is only for attaching methods to TextDocument type
  for easier OOP-style consumption for C# users.
  There are no other circular dependencies.
 *)

open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open System.Globalization

/// A TextDocument is an immutable data structure for manupilating text efficiently.
type TextDocument = {
  Buffer: PieceBuffer;
  Pieces: PieceTree;
  UndoStack: PieceTree list;
  RedoStack: PieceTree list;
  ShouldAddToHistory: bool;
}
  with
  member inline this.Lines                    = PieceTree.lines this.Pieces
  member inline this.Length                   = PieceTree.size this.Pieces

  member inline this.Insert(index, string)    = TextDocument.insert index string this
  member inline this.Delete(start, length)    = TextDocument.delete start length this
  member inline this.Prepend string           = TextDocument.prepend string this
  member inline this.Append  string           = TextDocument.append string this

  member inline this.Substring(start, length) = TextDocument.substring start length this
  member inline this.GetLine line             = TextDocument.getLine line this
  member inline this.Text()                   = TextDocument.text this

  member inline this.AddToHistory()           = TextDocument.addToHistory this
  member inline this.CanUndo                  = TextDocument.canUndo this
  member inline this.CanRedo                  = TextDocument.canRedo this
  member inline this.Undo()                   = TextDocument.undo this
  member inline this.Redo()                   = TextDocument.redo this

/// The TextDocument module provides functions for inserting into, deleting from and querying ranges of text.
[<RequireQualifiedAccess>]
module TextDocument =
  let empty = { 
    Buffer = PieceBuffer.empty; 
    Pieces = PieceTree.empty;
    UndoStack = [];
    RedoStack = [];
    ShouldAddToHistory = false;
  }

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

  let insert (index: int) (string: string) document =
    if String.IsNullOrEmpty string then
      document
    else
      let pcStart = PieceBuffer.size document.Buffer
      let (lineBreaks, charBreaks) = preprocessString string pcStart
      let buffer = PieceBuffer.append string charBreaks document.Buffer
      let pieces = PieceTree.insert index pcStart charBreaks.Length lineBreaks document.Pieces

      let undo =
        if document.ShouldAddToHistory then
          document.Pieces::document.UndoStack
        else
          document.UndoStack
      let redo = []
      { Buffer = buffer; Pieces = pieces; UndoStack = undo; RedoStack = redo; ShouldAddToHistory = false }

  let prepend (string: string) document =
    if String.IsNullOrEmpty string then
      document
    else
      let pcStart = PieceBuffer.size document.Buffer
      let (lineBreaks, charBreaks) = preprocessString string pcStart
      let buffer = PieceBuffer.append string charBreaks document.Buffer
      let pieces = PieceTree.prepend pcStart charBreaks.Length lineBreaks document.Pieces

      let undo =
        if document.ShouldAddToHistory then
          document.Pieces::document.UndoStack
        else
          document.UndoStack
      let redo = []
      { Buffer = buffer; Pieces = pieces; UndoStack = undo; RedoStack = redo; ShouldAddToHistory = false }
    
  let append (string: string) document =
    if String.IsNullOrEmpty string then
      document
    else
      let pcStart = PieceBuffer.size document.Buffer
      let (lineBreaks, charBreaks) = preprocessString string pcStart
      let buffer = PieceBuffer.append string charBreaks document.Buffer
      let pieces = PieceTree.append pcStart charBreaks.Length lineBreaks document.Pieces

      let undo =
        if document.ShouldAddToHistory then
          document.Pieces::document.UndoStack
        else
          document.UndoStack
      let redo = []
      { Buffer = buffer; Pieces = pieces; UndoStack = undo; RedoStack = redo; ShouldAddToHistory = false }

  let delete (start: int) (length: int) document =
    if length <= 0 then
      document
    else
      let pieces = PieceTree.delete start length document.Pieces

      let undo =
        if document.ShouldAddToHistory then
          document.Pieces::document.UndoStack
        else
          document.UndoStack
      let redo = []
      { document with Pieces = pieces; UndoStack = undo; RedoStack = redo; ShouldAddToHistory = false }

  let create string = append string empty

  let substring (start: int) (length: int) document =
    PieceTree.substring start length document.Pieces document.Buffer

  let getLine (line: int) document =
    PieceTree.getLine line document.Pieces document.Buffer

  let text document =
    PieceTree.getText document.Pieces document.Buffer

  let canUndo document =
    match document.UndoStack with
    | [] -> false
    | _ -> true

  let canRedo document =
    match document.RedoStack with
    | [] -> false
    | _ -> true

  let undo document = 
    if canUndo document then
      let redo = document.Pieces::document.RedoStack
      let current = document.UndoStack.Head
      let undo = document.UndoStack.Tail
      { document with Pieces = current; UndoStack = undo; RedoStack = redo; ShouldAddToHistory = false; }
    else
      document

  let redo document =
    if canRedo document then
      let current = document.RedoStack.Head
      let redo = document.RedoStack.Tail
      let undo = document.Pieces::document.UndoStack
      { document with Pieces = current; UndoStack = undo; RedoStack = redo; ShouldAddToHistory = false; }
    else
      document

  let addToHistory document =
    { document with ShouldAddToHistory = true; }

  type JsonPiece = 
    {
      Start: int;
      Length: int;
      LineBreaks: int ResizeArray;
    }

  type JsonDocument = 
    {
      Buffer: string;
      Pieces: JsonPiece ResizeArray;
      (* Below stacks are arrays of array, where nested array is a single tree and outer array is stacl. *)
      UndoStack: JsonPiece ResizeArray ResizeArray;   
      RedoStack: JsonPiece ResizeArray ResizeArray; 
    }

  let serialise (document: TextDocument) filePath =
    async {
      (* Helper function to add pieces in a given tree to a given array. *)
      let deserialiseTree tree (arr: JsonPiece ResizeArray) =
        PieceTree.foldPieces (fun _ start length lines ->
          let piece = { Start = start; Length = length; LineBreaks = ResizeArray(lines); }
          arr.Add piece
        ) () tree

      let deserialiseTreeList treeList (arr: JsonPiece ResizeArray ResizeArray) =
        List.fold (fun _ tree -> 
          let treeArr = ResizeArray(PieceTree.ht tree) (* Create new local array just for the current tree. *)
          deserialiseTree tree treeArr
          arr.Add treeArr
        ) () treeList

      (* Convert buffer to a single contiguuus string. *)
      let sb = StringBuilder(PieceBuffer.size document.Buffer)
      PieceBuffer.foldStrings (fun str -> sb.Append str |> ignore) () document.Buffer
      let buffer = sb.ToString()

      (* Convert pieces to a ResizeArray of JsonPiece. *)
      let pieces = ResizeArray(PieceTree.ht document.Pieces) 
      deserialiseTree document.Pieces pieces

      (* Also convert undo and redo stacks to a ResizeArray of JsonPiece. *)
      let undoArray = ResizeArray()
      let redoArray = ResizeArray()
      deserialiseTreeList document.UndoStack undoArray
      deserialiseTreeList document.RedoStack redoArray

      (* Construct record of various converted pieces. *)
      let jsonDoc = {
        Buffer = buffer;
        Pieces = pieces;
        UndoStack = undoArray;
        RedoStack = redoArray;
      }

      use createStream = File.Create filePath
      do! JsonSerializer.SerializeAsync(createStream, jsonDoc) |> Async.AwaitTask
    }

