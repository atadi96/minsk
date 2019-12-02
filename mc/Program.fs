// Learn more about F# at http://fsharp.org

open System

type NumberTokenData = int

type SyntaxKind =
    | NumberToken
    | WhiteSpaceToken
    | PlusToken
    | MinusToken
    | StarToken
    | SlashToken
    | OpenParenthesisToken
    | CloseParenthesisToken
    | BadToken
    | EndOfFileToken
    | NumberExpression
    | BinaryExpression
    | ParenthesizedExpression

type ISyntaxNode =
    abstract member Kind : SyntaxKind
    abstract member Children : ISyntaxNode seq

module SyntaxNode =
    let from (node: ISyntaxNode) = node
    let kind (node: ISyntaxNode) = node.Kind
    let children (node: ISyntaxNode) = node.Children

type SyntaxToken(kind: SyntaxKind, position: int, text: string, value: obj) =
    member __.Kind = kind
    member __.Position = position
    member __.Text = text
    member __.Value = value

    interface ISyntaxNode with
        member this.Kind = this.Kind
        member __.Children = Seq.empty

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
        let token = SyntaxToken(kind, position, text, null)
        next()
        token
    seq {
        while true do
            if position >= text.Length then
                yield SyntaxToken(EndOfFileToken, position, "\0", null)
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
                yield SyntaxToken(WhiteSpaceToken, start, tokenText, null)
            | ch when Char.IsDigit ch ->
                let start = position
                while currentChar () |> Char.IsDigit do
                    next()
                let length = position - start
                let tokenText = text.Substring(start, length)
                yield SyntaxToken(NumberToken, start, tokenText, Int32.Parse tokenText)
            | _ ->
                let badToken = SyntaxToken(BadToken, position, currentChar() |> string, null)
                next()
                yield badToken
    }

type ExpressionSyntax =
    | NumberExpression of SyntaxToken
    | BinaryExpression of ExpressionSyntax * SyntaxToken * ExpressionSyntax
    interface ISyntaxNode with
        member this.Kind =
            match this with
            | NumberExpression _ -> SyntaxKind.NumberExpression
            | BinaryExpression _ -> SyntaxKind.BinaryExpression
        member this.Children =
            match this with
            | NumberExpression token -> token :> ISyntaxNode |> Seq.singleton
            | BinaryExpression (l,op,r) ->
                let node = SyntaxNode.from
                [ node l; node op; node r ] |> Seq.ofList

type SyntaxNode =
    | ExpressionSyntax of ExpressionSyntax
    interface ISyntaxNode with
        member this.Kind =
            match this with
            | ExpressionSyntax exp -> SyntaxNode.kind exp
        member this.Children =
            match this with
            | ExpressionSyntax exp -> SyntaxNode.children exp

type SyntaxElement =
    | SyntaxNodeElement of SyntaxNode
    | ExpressionSyntaxElement of ExpressionSyntax
    | SyntaxTokenElement of SyntaxToken
    interface ISyntaxNode with
        member this.Kind =
            match this with
            | SyntaxNodeElement node -> SyntaxNode.kind node
            | ExpressionSyntaxElement exp -> SyntaxNode.kind exp
            | SyntaxTokenElement token -> SyntaxNode.kind token
        member this.Children =
            match this with
            | SyntaxNodeElement node -> SyntaxNode.children node
            | ExpressionSyntaxElement exp -> SyntaxNode.children exp
            | SyntaxTokenElement token -> SyntaxNode.children token

let prettyPrint (node: ISyntaxNode): string seq =
    let rec pp (nodes: ISyntaxNode list) parentIndent =
        let valueText (node: ISyntaxNode) =
            match node with
            | :? SyntaxToken as t when not (isNull t.Value) -> sprintf " %A" t.Value
            | _ -> ""
        match nodes with
        | [] -> Seq.empty
        | [ node ] ->
            let marker = "└──"
            let valueText = valueText node
            seq {
                yield sprintf "%s%s%A%s" parentIndent marker node.Kind valueText
                let indent = parentIndent + "   "
                yield! pp (node.Children |> Seq.toList) indent
            }
        | node :: nodes ->
            let marker = "├──"
            let valueText = valueText node
            seq {
                yield sprintf "%s%s%A%s" parentIndent marker node.Kind valueText
                let indent = parentIndent + "| "
                yield! pp (node.Children |> Seq.toList) indent
                yield! pp nodes parentIndent
            }
    pp [ node ] ""


module Parser =
    type private ParserState =
        {
            tokens: SyntaxToken []
            currentTokenIndex: int
            diagnostics: string list
        }
        member this.Peek n =
            let index = this.currentTokenIndex + n
            if index < this.tokens.Length then
                this.tokens.[index]
            else this.tokens.[this.tokens.Length - 1]
        member this.Current = this.Peek 0
        member this.Skip =
            { this with
                currentTokenIndex = this.currentTokenIndex + 1
            }
        member this.Diagnostics text = { this with diagnostics = this.diagnostics @ [ text ] }
        member this.NextToken = this.Current, this.Skip
        member this.Match (kind: SyntaxKind) =
            if this.Current.Kind = kind then
                this.NextToken
            else
                SyntaxToken(kind, this.Current.Position, null, null), 
                this.Skip.Diagnostics (sprintf "ERROR: Unexpected token <%A>, expected <%A>" this.Current.Kind kind)

    let private initState (tokens: SyntaxToken seq) =
        let tokens =
            tokens
            |> Seq.filter (fun token ->
                match token.Kind with
                | BadToken
                | WhiteSpaceToken -> false
                | _ -> true
            )
            |> fun tokens ->
                seq {
                    let mutable go = true
                    for token in tokens do
                        yield (token,go)
                        if token.Kind = EndOfFileToken then go <- false
                }
            |> Seq.takeWhile snd
            |> Seq.map fst
            |> Seq.toArray
        {
            tokens = tokens
            diagnostics = []
            currentTokenIndex = 0
        }    
    type Parser<'T> = private PS of (ParserState -> 'T * ParserState)

    type ParserBuilder() =
        member __.Return x = PS (fun s -> x, s)
        member __.Bind (PS p: Parser<'T>, f: 'T -> Parser<'U>) =
            PS (fun s ->
                let (t,s') = p s
                let (PS g) = f t
                g s'
            )
        member __.ReturnFrom x = x

    let run (PS parser) tokens =
        let (result,state) = tokens |> initState |> parser
        result,state.diagnostics
    let peek n = PS (fun s -> s.Peek n, s)
    let currentToken = PS (fun s -> s.Current, s)
    let nextToken = PS (fun s -> s.NextToken)
    let expect kind = PS (fun s -> s.Match kind)
    let diagnostics text = PS (fun s -> (), s.Diagnostics text)
    
    let parser = ParserBuilder()

module MinskParser =
    open Parser
    let parsePrimary =
        parser {
            let! token = expect NumberToken
            return NumberExpression token
        }
    let parseBinary =
        let rec binary (left:ExpressionSyntax): Parser<ExpressionSyntax> =
            parser {
                let! current = currentToken
                match current.Kind with
                | PlusToken
                | MinusToken ->
                    let! op = nextToken
                    let! right = parsePrimary
                    return! binary (BinaryExpression (left, op, right))
                | _ -> return left
            }
        parser {
            let! left = parsePrimary
            return! binary left
        }
    let private parser =
        parser {
            let! bin = parseBinary
            let! _end = expect EndOfFileToken
            return ExpressionSyntaxElement bin
        }
    let parse tokens = run parser tokens

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    while true do
        printf "> "
        let line = Console.ReadLine()
        
        if not (String.IsNullOrWhiteSpace(line)) then
            let tokens = lex line
            (*
            tokens
            |> Seq.takeWhile (fun token -> token.Kind <> EndOfFileToken)        
            |> Seq.iter (fun token ->
                printfn "%A: '%s'" token.Kind token.Text
            )
            *)
            let (tree,diag) = MinskParser.parse tokens
            for msg in diag do
                printfn "%s" msg
            if diag.Length = 0 then
                tree |> prettyPrint |> Seq.iter (printfn "%s")

    0 // return an integer exit code
