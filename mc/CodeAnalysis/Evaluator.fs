module internal CodeAnalysis.Evaluator

open CodeAnalysis.Binding

let rec evaluateExpression (es: BoundExpression): obj =
    match es with
    | BoundBinaryExpression (left,op,right) ->
        let left = evaluateExpression left :?> int
        let right = evaluateExpression right :?> int
        match op with
        | Addition -> left + right :> obj
        | Substraction -> left - right :> obj
        | Multiplication -> left * right :> obj
        | Division -> left / right :> obj
        | _ -> failwith (sprintf "Unsupported binary operator '%A'." op)
    | BoundLiteralExpression value ->
        if isNull value then
            0 :> obj
        else value
    | BoundUnaryExpression (op,exp) ->
        let operand = evaluateExpression exp :?> int
        match op with
        | Identity -> operand :> obj
        | Negation -> -operand :> obj
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