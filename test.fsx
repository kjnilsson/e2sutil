[<AutoOpen>]
module Test 
let assertEqual expected actual =
    if expected <> actual then
        failwith (sprintf "Test Failed.\r\nExpected: %A\r\nGot: %A" expected actual)

let test (tests : (string * (unit -> unit)) list) =
    tests
    |> List.map (fun (n, f) ->
        try 
            f()
            Choice1Of2 n with
        | ex -> Choice2Of2 (n, ex))
    |> List.iter (
        function
        | Choice1Of2 name ->
            printfn "OK: %s" name
        | Choice2Of2 (name, ex) ->
            printfn "ERR: %s failed with %s" name ex.Message)


#if INTERACTIVE

test
    [ "first",  fun () -> assertEqual 1 2
      "second", fun () -> assertEqual 1 1 ]

#endif
            

        
        