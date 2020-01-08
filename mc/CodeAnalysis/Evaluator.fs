module CodeAnalysis.Evaluator

open Syntax

let rec evaluateExpression (es: ExpressionSyntax) =
    match es with
    | BinaryExpression (left,op,right) ->
        let left = evaluateExpression left
        match op.Kind with
        | PlusToken -> left + evaluateExpression right
        | MinusToken -> left - evaluateExpression right
        | StarToken -> left * evaluateExpression right
        | SlashToken -> left / evaluateExpression right
        | _ -> failwith (sprintf "unsupported binary operator '%A' represented by text '%s'" op.Kind op.Text)
    | NumberExpression token ->
        match token.Kind with
        | NumberToken -> token.Value :?> int
        | _ -> failwith (sprintf "unsupported number expression '%A' represented by text '%s'" token.Kind token.Text)
    | ParenthesizedExpression (_,exp,_) -> evaluateExpression exp

let evaluate (se: SyntaxElement) =
    match se with
    | SyntaxNodeElement (n:SyntaxNode) ->
        match n with
        | ExpressionSyntax (es:ExpressionSyntax) -> evaluateExpression es
    | ExpressionSyntaxElement es -> evaluateExpression es
    | SyntaxTokenElement token -> failwith "cant even :'("
