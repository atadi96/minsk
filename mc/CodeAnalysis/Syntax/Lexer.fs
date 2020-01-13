module internal CodeAnalysis.Syntax.Lexer

open Syntax
open System

type LexError = string

let lex (text: string) : (SyntaxToken * LexError option) seq =
    let mutable position = 0
    let next() = position <- position + 1
    let peek offset =
        let index = position + offset
        if index >= text.Length then
            char 0
        else text.Chars index
    let currentChar() = peek 0
    let lookahead() = peek 1
    let charToken kind text =
        let token = SyntaxToken(kind, position, text, null)
        next()
        token
    seq {
        while true do
            if position >= text.Length then
                yield SyntaxToken(EndOfFileToken, position, "\0", null), None
            else        
            match currentChar() with
            | '+' -> yield charToken PlusToken "+", None
            | '-' -> yield charToken MinusToken "-", None
            | '*' -> yield charToken StarToken "*", None
            | '/' -> yield charToken SlashToken "/", None
            | '(' -> yield charToken OpenParenthesisToken "(", None
            | ')' -> yield charToken CloseParenthesisToken ")", None
            | '!' -> yield charToken BangToken "!", None
            | '&' when lookahead() = '&' ->
                let position = position
                next ()
                next ()
                yield SyntaxToken(AmpersandAmpersandToken, position, "&&", null), None
            | '|' when lookahead() = '|' ->
                let position = position
                next ()
                next ()
                yield SyntaxToken(PipePipeToken, position, "||", null), None
            | ch when Char.IsWhiteSpace ch ->
                let start = position
                while currentChar () |> Char.IsWhiteSpace do
                    next()
                let length = position - start
                let tokenText = text.Substring(start, length)
                yield SyntaxToken(WhiteSpaceToken, start, tokenText, null), None
            | ch when Char.IsDigit ch ->
                let start = position
                while currentChar () |> Char.IsDigit do
                    next()
                let length = position - start
                let tokenText = text.Substring(start, length)
                yield SyntaxToken(NumberToken, start, tokenText, Int32.Parse tokenText), None
            | ch when Char.IsLetter ch ->
                let start = position
                while currentChar () |> Char.IsLetter do
                    next()
                let length = position - start
                let tokenText = text.Substring(start, length)
                let kind = SyntaxFacts.getKeywordKind tokenText
                yield SyntaxToken(kind, start, tokenText, null), None
            | _ ->
                let currentChar = currentChar()
                let badToken = SyntaxToken(BadToken, position, currentChar |> string, null)
                next()
                yield badToken, Some (sprintf "ERROR: bad character in input: '%c'" currentChar)
    }