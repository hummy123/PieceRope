open PieceRope
open PieceRope.PieceRope

module Program =
    [<EntryPoint>]
    let main _ =  
        let str = "abcdefghij"
        let rope = PieceRope.create str
    
        let rope = rope.Insert(4, "\n")
        let str = str.Insert(4, "\n")
        let strLines = str.Split("\n")
        rope.GetLine 0

        0