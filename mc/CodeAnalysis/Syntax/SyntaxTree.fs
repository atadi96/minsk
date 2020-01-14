module CodeAnalysis.Syntax.SyntaxTree

let parse (text: string) =
    text
    |> Lexer.lex
    |> MinskParser.parse