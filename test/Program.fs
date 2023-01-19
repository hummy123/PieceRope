open PieceRope


module Program =
    [<EntryPoint>]
    let main _ =  
        let rope = PieceRope.create "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
        for i in [9] do
            let line = PieceRope.getLine i rope
            printfn "%s" line
        0