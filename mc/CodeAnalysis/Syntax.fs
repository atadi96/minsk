module CodeAnalysis.Syntax

type NumberTokenData = int

type SyntaxKind =
    // Tokens
    | BadToken
    | EndOfFileToken
    | WhiteSpaceToken
    | NumberToken
    | PlusToken
    | MinusToken
    | StarToken
    | SlashToken
    | OpenParenthesisToken
    | CloseParenthesisToken

    // Expressions
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

type ExpressionSyntax =
    | LiteralExpression of SyntaxToken
    | BinaryExpression of ExpressionSyntax * SyntaxToken * ExpressionSyntax
    | ParenthesizedExpression of SyntaxToken * ExpressionSyntax * SyntaxToken
    interface ISyntaxNode with
        member this.Kind =
            match this with
            | LiteralExpression _ -> SyntaxKind.NumberExpression
            | BinaryExpression _ -> SyntaxKind.BinaryExpression
            | ParenthesizedExpression _ -> SyntaxKind.ParenthesizedExpression
        member this.Children =
            match this with
            | LiteralExpression token -> token :> ISyntaxNode |> Seq.singleton
            | BinaryExpression (l,op,r) ->
                let node = SyntaxNode.from
                [ node l; node op; node r ] |> Seq.ofList
            | ParenthesizedExpression (oPar,exp,cPar) ->
                let node = SyntaxNode.from
                [ node oPar; node exp; node cPar ]
                |> Seq.ofList

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
