module internal CodeAnalysis.SyntaxFacts

open Syntax

let precedence (kind: SyntaxKind) =
    match kind with
    | StarToken
    | SlashToken -> 2
    | PlusToken
    | MinusToken -> 1
    | _ -> 0