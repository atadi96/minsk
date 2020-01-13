module internal CodeAnalysis.Syntax.SyntaxFacts

open Syntax

let binaryPrecedence (kind: SyntaxKind) =
    match kind with
    | StarToken
    | SlashToken -> 4
    | PlusToken
    | MinusToken -> 3
    | AmpersandAmpersandToken -> 2
    | PipePipeToken -> 1
    | _ -> 0

let unaryPrecedence (kind: SyntaxKind) =
    match kind with
    | PlusToken
    | MinusToken
    | BangToken -> 5
    | _ -> 0

let getKeywordKind (keyword: string): Syntax.SyntaxKind =
    match keyword with
    | "true" -> TrueKeyword
    | "false" -> FalseKeyword
    | _ -> IdentifierToken