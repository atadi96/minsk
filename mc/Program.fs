// Learn more about F# at http://fsharp.org

open System
open CodeAnalysis
            
let withColor c f =
    let color = Console.ForegroundColor
    Console.ForegroundColor <- c
    f()
    Console.ForegroundColor <- color

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    let mutable showTree = false
    while true do
        printf "> "
        let line = Console.ReadLine()
        match line with
        | "#showTree" ->
            showTree <- not showTree
            if showTree then
                "Showing parse tree"
            else
                "Not showing parse tree"
            |> printfn "%s"
        | "#cls" ->
            Console.Clear()
        | line when not (String.IsNullOrWhiteSpace(line)) ->
            let (tree,diag) = SyntaxTree.parse line
            withColor ConsoleColor.DarkGray (fun () ->
                if showTree then
                    tree |> Syntax.prettyPrint |> Seq.iter (printfn "%s")
            )
            if diag |> List.isEmpty |> not then
                withColor ConsoleColor.DarkRed (fun () ->
                    for msg in diag do
                        printfn "%s" msg
                )
            else printfn "%i" (Evaluator.evaluate tree)
        | _ -> ()

    0 // return an integer exit code
