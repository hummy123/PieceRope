open PieceRope
open PieceRope.PieceRope

module Program =
    [<EntryPoint>]
    let main _ =  
        let str = "Lorem ipsum\ndolor sit amet,\nconsectetur\nadipiscing elit. \nAenean ornare, \nlacus vitae \ntempor pretium,\nleo nulla\nsollicitudin elit,\nin ultrices mi dui et\nipsum. Cras condimentum\npurus in metus \nsodales tincidunt. Praesent"
        let initRope = PieceRope.create str

        // this causes some metadata issues because without it 57 works
        let rope = initRope.Delete(11, 17)
        let str = str.Remove(11, 17)
        printfn "same after first delete? %A" (str = rope.Text())

        let rope = rope.Delete(12, 1)
        let str = str.Remove(12, 1)
        printfn "same after second delete? %A" (str = rope.Text())
        printfn "\nstr:\n%A" str
        printfn "\nrope:\n%A" (rope.Text())
        0