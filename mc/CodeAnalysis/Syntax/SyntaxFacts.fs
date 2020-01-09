module internal CodeAnalysis.Syntax.SyntaxFacts

open Syntax

let binaryPrecedence (kind: SyntaxKind) =
    match kind with
    | StarToken
    | SlashToken -> 2
    | PlusToken
    | MinusToken -> 1
    | _ -> 0

let unaryPrecedence (kind: SyntaxKind) =
    match kind with
    | PlusToken
    | MinusToken -> 3
    | _ -> 0