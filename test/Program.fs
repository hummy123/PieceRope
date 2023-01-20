open PieceRope
open PieceRope.PieceRope

module Program =
    [<EntryPoint>]
    let main _ =  
        let rope = PieceRope.create "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
    
        // Delete first line break and see if we can get expected string from result.
        let rope = rope.Delete(11, 1)
        let rope = rope.Delete(38, 1) 

        printfn "%s" (rope.GetLine 0)
        0