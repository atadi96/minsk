module CodeAnalysis.SyntaxTree

let parse (text: string) =
    text
    |> Lexer.lex
    |> MinskParser.parse