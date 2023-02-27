namespace HumzApps.TextDocument

open System.Text
open System.IO
open System.Text
open System.Text.Json

/// A JsonPiece is a type used internally for serialisation. 
/// It is an implementation details exposed publicly because System.Text.Json would not work otherwise.
type JsonPiece = 
  {
    Start: int;
    Length: int;
    LineBreaks: int array;
  }

/// A JsonDocument is a type used internally for serialisation. 
/// It is an implementation details exposed publicly because System.Text.Json would not work otherwise.
type JsonDocument = 
  {
    Buffer: string array;
    Pieces: JsonPiece array;
    Current: int array;
    (* Below stacks are arrays of array, where nested array is a single tree and outer array is stack. *)
    UndoStack: int array array;   
    RedoStack: int array array; 
  }

/// Implementation of a weight balanced tree, containing all JsonPieces used.
type WbTree =
  | WE
  | WT of JsonPiece * int * WbTree * WbTree

module PieceConverter =
  let private WEIGHT = 4
  let private RATIO  = 2

  let inline private size t =
    match t with
    | WE -> 0
    | WT(_, count, _, _) -> count

  (* Smart constructor. *)
  let inline private N v l r =
    WT(v, 1 + size l + size r, l, r)

  let inline private singleL a x r =
    match r with
    | WT(b, _, y, z) -> N b (N a x y) z
    | _ -> failwith "unexpected singleL"

  let inline private doubleL a x r =
    match r with
    | WT(c, _, WT(b, _, y1, y2), z) -> N b (N a x y1) (N c y2 z)
    | _ -> failwith "unexpected doubleL"

  let inline private singleR b l z =
    match l with
    | WT(a, _, x, y) -> N a x (N b y z)
    | _ -> failwith "unexpected singleR"

  let inline private doubleR c l z =
    match l with
    | WT(a, _, x, WT(b, _, y1, y2)) -> N b (N a x y1) (N c y2 z)
    | _ -> failwith "unexpected doubleR"

  let inline private T' v l r =
    let ln = size l
    let rn = size r
    if ln + rn < 2 then
      N v l r
    elif rn > WEIGHT * ln then
      match r with
      | WT(_, _, rl, rr) ->
        let rln = size rl
        let rrn = size rr
        if rln < rrn then
          singleL v l r
        else
          doubleL v l r
      | WE -> failwith "unexpected T'"
    elif ln > WEIGHT * rn then
      match l with 
      | WT(_, _, ll, lr) ->
        let lln = size ll
        let lrn = size lr
        if lrn < lln then
          singleR v l r
        else
          doubleR v l r
      | WE -> failwith "unexpected T'"
    else
      N v l r

  let rec private add x = function
    | WE -> WT(x, 1, WE, WE)
    | WT(v, _, l, r) as tree ->
      if x < v then 
        T' v (add x l) r
      elif x > v then 
        T' v l (add x r)
      else tree
    
  (* Finds the index of element x. *)
  let private rank x tree =
    let rec rnk acc = function
      | WE -> failwith "PieceSerialiser.rank: element not found"
      | WT(v, n, l, r) ->
          if x < v then
            rnk acc l
          elif x > v then
            rnk (acc + size l + 1) r
          else
            acc + size l
    rnk 0 tree

  let empty = WbTree.WE

  let rec private fold folder state = function
    | WE -> state
    | WT(piece, _, l, r) ->
        let x = fold folder state l
        let x = folder x piece
        fold folder x r

  [<Literal>]
  let private SB_LENGTH_LIMIT = 65535

  /// Converts pieces of a TextDocument into a JsonDocument type for serialisation.
  let convertToJsonDoc undoStack current redoStack buffer =
    (* Helper function to add pieces in a given PieceTree to a given WbTree. *)
    let deserialiseTree pcTree wbTree =
      PieceTree.foldPieces (fun acc start length lines ->
        let piece = { Start = start; Length = length; LineBreaks = lines; }
        add piece acc
      ) wbTree pcTree

    (* Helper function to add pieces in a list of PieceTrees to a given WbTree. *)
    let deserialiseTreeList treeList wbTree =
      List.fold (fun accWbTree pcTree -> 
        deserialiseTree pcTree accWbTree
      ) wbTree treeList

    (* Helper function to add pieces of given PieceTree to given array. *)
    let buildPcArray pcTree wbTree (arr: int ResizeArray) =
      PieceTree.foldPieces (fun _ start length lines ->
        let piece = {Start = start; Length = length; LineBreaks = lines;}
        rank piece wbTree |> arr.Add
      ) () current
      arr.ToArray()

    (* Helper function to convert undo/redo stack to array. *)
    let buildPcArrayList pcTreeList wbTree (arr: int array ResizeArray) =
      List.fold (fun _ pcTree ->
        let curArr = ResizeArray()
        buildPcArray pcTree wbTree curArr
        |> arr.Add
      ) () pcTreeList
      arr.ToArray()

    (* Create a WbTree containing all pieces. *)
    let wbTree = 
      deserialiseTree current empty
      |> deserialiseTreeList undoStack
      |> deserialiseTreeList redoStack

    (* Build array containing all pieces. *)
    let pieceArr = ResizeArray(size wbTree * 2)
    fold (fun _ piece ->
      pieceArr.Add piece
    ) () wbTree
    let pieceArr = pieceArr.ToArray()

    (* Build array containing index of pieces for different PieceTrees. *)
    let currentArr = 
      ResizeArray(PieceTree.size current)
      |> buildPcArray current wbTree

    let undoArr =
      ResizeArray()
      |> buildPcArrayList undoStack wbTree

    let redoArr =
      ResizeArray()
      |> buildPcArrayList redoStack wbTree

    let bufferArray = ResizeArray()
    let sb = StringBuilder(SB_LENGTH_LIMIT)
    PieceBuffer.foldStrings (fun str -> 
      if sb.Length > SB_LENGTH_LIMIT then
        bufferArray.Add (sb.ToString())
        sb.Clear() |> ignore
      sb.Append str |> ignore
    ) () buffer
    bufferArray.Add (sb.ToString())
    
    {
      Buffer = bufferArray.ToArray();
      Pieces = pieceArr;
      Current = currentArr;
      UndoStack = undoArr;
      RedoStack = redoArr;
    }

  let convertFromJsonDoc jsonDoc =
    (* Convert string array to PieceBuffer. *)
    let mutable buffer = PieceBuffer.empty
    for str in jsonDoc.Buffer do
      let (_, charBreaks) = PieceString.preprocessString str (PieceBuffer.size buffer)
      buffer <- PieceBuffer.append str charBreaks buffer

    (* Convert current JsonPieces to PieceTree. *)
    let mutable current = PieceTree.empty
    for pcIdx in jsonDoc.Current do 
      let pc = jsonDoc.Pieces[pcIdx]
      current <- PieceTree.insMax pc.Start pc.Length pc.LineBreaks current

    let mutable undoStack = []
    for treeIdx = jsonDoc.UndoStack.Length - 1 downto 0 do 
      let treePtrs = jsonDoc.UndoStack[treeIdx]
      let mutable tree = PieceTree.empty
      for pcIdx in treePtrs do
        let pc = jsonDoc.Pieces[pcIdx]
        tree <- PieceTree.insMax pc.Start pc.Length pc.LineBreaks tree
      undoStack <- tree::undoStack
      
    let mutable redoStack = []
    for treeIdx = jsonDoc.RedoStack.Length - 1 downto 0 do 
      let treePtrs = jsonDoc.RedoStack[treeIdx]
      let mutable tree = PieceTree.empty
      for pcIdx in treePtrs do
        let pc = jsonDoc.Pieces[pcIdx]
        tree <- PieceTree.insMax pc.Start pc.Length pc.LineBreaks tree
      redoStack <- tree::redoStack
      
    (buffer, current, undoStack, redoStack)    
