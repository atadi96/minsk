module internal CodeAnalysis.MinskParser

open Syntax
open Parser

let rec parsePrimary =
    parser {
        let! current = currentToken
        if current.Kind = OpenParenthesisToken then
            let! left = nextToken
            let! expression = parseExpression
            let! right = expect CloseParenthesisToken
            return ParenthesizedExpression (left,expression,right)
        else
            let! token = expect NumberToken
            return LiteralExpression token
    }
and parseFactor =
    let rec binaryFactor (left:ExpressionSyntax): Parser<ExpressionSyntax> =
        parser {
            let! current = currentToken
            match current.Kind with
            | StarToken
            | SlashToken ->
                let! op = nextToken
                let! right = parsePrimary
                return! binaryFactor (BinaryExpression (left, op, right))
            | _ -> return left
        }
    parser {
        let! left = parsePrimary
        return! binaryFactor left
    }
and parseTerm =
    let rec parseBinaryTerm (left:ExpressionSyntax): Parser<ExpressionSyntax> =
        parser {
            let! current = currentToken
            match current.Kind with
            | PlusToken
            | MinusToken ->
                let! op = nextToken
                let! right = parseFactor
                return! parseBinaryTerm (BinaryExpression (left, op, right))
            | _ -> return left
        }
    parser {
        let! left = parseFactor
        return! parseBinaryTerm left
    }
and private parseExpression =
    parser {
        return! parseTerm
    }
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