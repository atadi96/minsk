module internal CodeAnalysis.Evaluator

open CodeAnalysis.Binding

let rec evaluateExpression (es: BoundExpression) =
    match es with
    | BoundBinaryExpression (left,op,right) ->
        let left = evaluateExpression left
        match op with
        | Addition -> left + evaluateExpression right
        | Substraction -> left - evaluateExpression right
        | Multiplication -> left * evaluateExpression right
        | Division -> left / evaluateExpression right
        | _ -> failwith (sprintf "Unsupported binary operator '%A'." op)
    | BoundLiteralExpression value -> value :?> int
    | BoundUnaryExpression (op,exp) ->
        let operand = evaluateExpression exp;
        match op with
        | Identity -> operand
        | Negation -> -operand
        | other -> failwith (sprintf "Undexpected unary operator %A" other)
(*
let evaluate (se: SyntaxElement) =
    match se with
    | SyntaxNodeElement (n:SyntaxNode) ->
        match n with
        | ExpressionSyntax (es:ExpressionSyntax) -> evaluateExpression es
    | ExpressionSyntaxElement es -> evaluateExpression es
    | SyntaxTokenElement token -> failwith "cant even :'("
*)