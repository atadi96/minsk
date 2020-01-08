module CodeAnalysis.Parser

open Syntax

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
