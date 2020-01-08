// Learn more about F# at http://fsharp.org

open System
open CodeAnalysis

[<EntryPoint>]
let main _argv =
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
            Console.ForegroundColor <- ConsoleColor.DarkGray
            if showTree then
                tree |> Syntax.prettyPrint |> Seq.iter (printfn "%s")
            Console.ResetColor()
            if diag |> List.isEmpty |> not then
                Console.ForegroundColor <- ConsoleColor.DarkRed
                for msg in diag do
                    printfn "%s" msg
                Console.ResetColor()
            else printfn "%i" (Evaluator.evaluate tree)
        | _ -> ()

    0 // return an integer exit code
