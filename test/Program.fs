open PieceRope




module Program =
    [<EntryPoint>]
    let main _ =  
        let rope = PieceRope.create "abcde\nfghij"
        printfn "bef |%s| aft" <| PieceRope.getLine 1 rope

        let rope = PieceRope.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
        printfn "bef |%s| aft" <| PieceRope.getLine 10 rope
        0