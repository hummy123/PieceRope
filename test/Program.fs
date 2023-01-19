open PieceRope
open PieceRope.PieceRope

module Program =
    [<EntryPoint>]
    let main _ =  
        let rope = PieceRope.create "abcde\nfghij"
        let rope = rope.Delete(5, 1)
        printfn "%s" <| rope.GetLine 0
        0