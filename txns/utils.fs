namespace Txns

open System
open HumzApps.TextDocument

module Utils =
  let runTxns (arr: (int * int * string) array) =
    Array.fold (fun doc (pos, delNum, insStr) ->
      let doc =
        if delNum > 0 then
          TextDocument.delete pos delNum doc
        else
          doc

      if insStr <> "" then
        TextDocument.insert pos insStr doc
      else
        doc
    ) TextDocument.empty arr

