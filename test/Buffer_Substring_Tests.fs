module BufferSubstringTests

open System
open System.Collections
open Xunit
open PieceRope
open Buffer.Tree

[<Fact>]
let ``Can get different indices of a buffer with five characters`` () =
    let buffer = empty |> append "12345"
    Assert.Equal("", substring 0 0 buffer)
    Assert.Equal("1", substring 0 1 buffer)
    Assert.Equal("12", substring 0 2 buffer)
    Assert.Equal("123", substring 0 3 buffer)
    Assert.Equal("1234", substring 0 4 buffer)
    Assert.Equal("12345", substring 0 5 buffer)
    Assert.Equal("12345", substring 0 6 buffer) (* Specifying length higher than what we have in the buffer should return max. *)

    Assert.Equal("2345", substring 1 4 buffer)
    Assert.Equal("345", substring 2 3 buffer)
    Assert.Equal("45", substring 3 2 buffer)
    Assert.Equal("5", substring 4 1 buffer)
    Assert.Equal("", substring 5 5 buffer) (* Length number doesn't matter in this case. *)

    Assert.Equal("2", substring 1 1 buffer)
    Assert.Equal("23", substring 1 2 buffer)
    Assert.Equal("234", substring 1 3 buffer)

[<Fact>]
let ``Can get different substrings there are three nodes in buffer`` () =
    let str = (String.replicate 65535 "a")
    let buffer = Buffer.Tree.append str Buffer.Tree.empty
    let str2 = (String.replicate 65535 "b")
    let buffer = append str2 buffer
    let str3 = (String.replicate 65535 "c")
    let buffer = append str3 buffer
    
    (* Can get substring including end of first node and start of second. *)
    Assert.Equal("ab", substring 65534 2 buffer) 

    (* Can get substring including end of first node, whole of second, and start of third. *)
    let expected = "a" + (String.replicate 65535 "b") + "c"
    Assert.Equal(expected, substring 65534 65537 buffer)

    (* Can get substring including all nodes. *)
    Assert.Equal(str + str2 + str3, substring 0 (65535 * 3) buffer)
