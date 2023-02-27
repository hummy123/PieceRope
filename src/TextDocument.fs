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

  member inline this.Serialise filePath       = TextDocument.serialise filePath this

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

  let insert (index: int) (string: string) document =
    if String.IsNullOrEmpty string then
      document
    else
      let pcStart = PieceBuffer.size document.Buffer
      let (lineBreaks, charBreaks) = PieceString.preprocessString string pcStart
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
      let (lineBreaks, charBreaks) = PieceString.preprocessString string pcStart
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
      let (lineBreaks, charBreaks) = PieceString.preprocessString string pcStart
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

  let serialise (filePath: string) document =
    async {
      let jsonDoc = 
        PieceConverter.convertToJsonDoc document.UndoStack document.Pieces document.RedoStack document.Buffer
      use createStream = File.Create filePath
      do! JsonSerializer.SerializeAsync(createStream, jsonDoc) |> Async.AwaitTask
    }

  let deserialise (filePath: string) = 
    async {
      let! jsonString = File.ReadAllTextAsync(filePath) |> Async.AwaitTask
      let jsonDoc = JsonSerializer.Deserialize<HumzApps.TextDocument.JsonDocument>(jsonString)
      let (buffer, pieces, undoStack, redoStack) = PieceConverter.convertFromJsonDoc jsonDoc
      return {
        Buffer = buffer;
        Pieces = pieces;
        UndoStack = undoStack;
        RedoStack = redoStack;
        ShouldAddToHistory = false;
      }
    }

