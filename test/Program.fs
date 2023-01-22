open PieceRope
open PieceRope.PieceRope
open System

module Program =
    [<EntryPoint>]
    let main _ =  
        let str = "abcdefghij"
        let rope = PieceRope.create str
    
        let rope = rope.Insert(4, "\n")
    
        let line = rope.GetLine 0
        0