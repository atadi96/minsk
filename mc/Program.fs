// Learn more about F# at http://fsharp.org

open System

type NumberTokenData = int

type SyntaxKind =
    | NumberToken of NumberTokenData
    | WhiteSpaceToken
    | PlusToken
    | MinusToken
    | StarToken
    | SlashToken
    | OpenParenthesisToken
    | CloseParenthesisToken
    | BadToken
    | EndOfFileToken

type SyntaxToken(kind: SyntaxKind, position: int, text: string) =
    member __.Kind = kind
    member __.Position = position
    member __.Text = text
(*
let lex' (text: string): SyntaxToken seq =
    let lexSame (pred: char -> bool) (chars: (int * char) seq): string * (int * char) seq
        *)

let lex (text: string) : SyntaxToken seq =
    let mutable position = 0
    let next() = position <- position + 1
    let currentChar() =
        if position >= text.Length then
            char 0
        else text.Chars position
    let charToken kind text =
        let token = SyntaxToken(kind, position, text)
        next()
        token
    seq {
        while true do
            if position >= text.Length then
                yield SyntaxToken(EndOfFileToken, position, "\0")
            else        
            match currentChar() with
            | '+' -> yield charToken PlusToken "+"
            | '-' -> yield charToken MinusToken "-"
            | '*' -> yield charToken StarToken "*"
            | '/' -> yield charToken SlashToken "/"
            | '(' -> yield charToken OpenParenthesisToken "("
            | ')' -> yield charToken CloseParenthesisToken ")"
            | ch when Char.IsWhiteSpace ch ->
                let start = position
                while currentChar () |> Char.IsWhiteSpace do
                    next()
                let length = position - start
                let tokenText = text.Substring(start, length)
                yield SyntaxToken(WhiteSpaceToken, start, tokenText)
            | ch when Char.IsDigit ch ->
                let start = position
                while currentChar () |> Char.IsDigit do
                    next()
                let length = position - start
                let tokenText = text.Substring(start, length)
                yield SyntaxToken(NumberToken (Int32.Parse tokenText), start, tokenText)
            | _ ->
                let badToken = SyntaxToken(BadToken, position, currentChar() |> string)
                next()
                yield badToken
    }

type ExpressionSyntax =
    | NumberExpression of SyntaxToken
    | BinaryExpression of ExpressionSyntax * SyntaxToken * ExpressionSyntax

type SyntaxNode =
    | ExpressionSyntax of ExpressionSyntax

type SyntaxElement =
    | SyntaxNodeElement of SyntaxNode
    | ExpressionSyntaxElement of ExpressionSyntax
    | SyntaxTokenElement of SyntaxToken

let rec children (element: SyntaxElement): SyntaxElement seq =
    match element with
    | SyntaxNodeElement (ExpressionSyntax expr) -> children (ExpressionSyntaxElement expr)
    | ExpressionSyntaxElement expr ->
        match expr with
        | NumberExpression syn -> SyntaxTokenElement syn |> Seq.singleton
        | BinaryExpression (left, op, right) ->
            [   ExpressionSyntaxElement left
                SyntaxTokenElement op
                ExpressionSyntaxElement right ]
            |> List.toSeq
    | SyntaxTokenElement _ -> Seq.empty

let prettyPrint (indent: string) (element: SyntaxElement) =
    printf "%s" indent
    match element with
    | SyntaxNodeElement node ->
        printfn "%A" node
    | ExpressionSyntaxElement expr ->
        printfn "%A" expr

type Parser(text: string) =
    let lexer = lex text
    let tokens =
        lexer
        |> Seq.filter (fun token ->
            match token.Kind with
            | BadToken
            | WhiteSpaceToken -> false
            | _ -> true
        )
        |> Seq.pairwise
        |> Seq.takeWhile (fun (a,b) ->
            match a.Kind with
            | EndOfFileToken -> false
            | _ -> true
        )
        |> Seq.map fst
        |> Seq.toArray
    let mutable position = 0
    let peek n =
        let index = position + n
        if index >= tokens.Length then
            tokens.[tokens.Length - 1]
        else
            tokens.[index]
    let current() = peek 0
    let nextToken() =
        let token = current()
        position <- position + 1
        token
    let requireSyntaxKind (kind: SyntaxKind) =
        if current().Kind = kind then
            nextToken()
        else SyntaxToken(kind, current().Position, "")
    let requireNumberToken () =
        match current().Kind with
        | NumberToken _ -> nextToken()
        | _ -> SyntaxToken(NumberToken 0, current().Position, "")

    member __.ParsePrimaryExpression(): ExpressionSyntax =
        let token = requireNumberToken ()
        NumberExpression token
    member this.Parse() =
        let mutable left = this.ParsePrimaryExpression()

        while current().Kind = PlusToken || current().Kind = MinusToken do
            let operator = nextToken()
            let right = this.ParsePrimaryExpression()
            left <- BinaryExpression (left, operator, right)



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    while true do
        printf "> "
        let line = Console.ReadLine()
        
        if not (String.IsNullOrWhiteSpace(line)) then
            let tokens = lex line

            tokens
            |> Seq.takeWhile (fun token -> token.Kind <> EndOfFileToken)        
            |> Seq.iter (fun token ->
                printfn "%A: '%s'" token.Kind token.Text
            )

    0 // return an integer exit code
