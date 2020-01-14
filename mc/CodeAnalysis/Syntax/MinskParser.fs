module internal CodeAnalysis.Syntax.MinskParser

open Syntax
open Parser

#nowarn "40" // recursive value definition of the parser
let rec parsePrimary =
    parser {
        let! current = currentToken
        match current.Kind with
        | OpenParenthesisToken ->
            let! left = nextToken
            let! expression = parseExpression
            let! right = expect CloseParenthesisToken
            return ParenthesizedExpression (left,expression,right)
        | TrueKeyword
        | FalseKeyword ->
            let! keywordToken = nextToken
            let value = keywordToken.Kind = TrueKeyword
            return LiteralExpression (keywordToken, value)
        | _ ->
            let! token = expect NumberToken
            return LiteralExpression (token, token.Value)
    }
and parseExpression =
    let rec parseExpression parentPrecedence =
        parser {
            let! current = currentToken
            let! left =
                match current.Kind |> SyntaxFacts.unaryPrecedence with
                | p when p <> 0 && p >= parentPrecedence ->
                    parser {
                        let! operator = nextToken
                        let! operand = parseExpression p
                        return UnaryExpression (operator,operand)
                    }
                | _ -> parsePrimary
            let rec parseHigherPrecedenceTerm left =
                parser {
                    let! current = currentToken
                    let tokenPrecedence = current |> SyntaxNode.kind |> SyntaxFacts.binaryPrecedence
                    if tokenPrecedence = 0 || tokenPrecedence <= parentPrecedence then
                        return left
                    else
                        let! operatorToken = nextToken
                        let! right = parseExpression tokenPrecedence
                        return! parseHigherPrecedenceTerm (BinaryExpression(left,operatorToken,right))
                }
            return! parseHigherPrecedenceTerm left
        }
    parseExpression 0
let parseProgram =
    parser {
        let! exp = parseExpression
        let! _end = expect EndOfFileToken
        return ExpressionSyntaxElement exp
    }
let private parser tokens: SyntaxElement * string list = run parseProgram tokens

let parse (tokens: seq<SyntaxToken * Lexer.LexError option>)=
    let mutable lexerDiag = []
    tokens
    |> Seq.map (fun (token,err) ->
        match err with
        | None -> () 
        | Some lexErr -> lexerDiag <- lexErr :: lexerDiag
        token
    )
    |> parser
    |> fun (s,diag) ->
        s, List.rev lexerDiag @ diag