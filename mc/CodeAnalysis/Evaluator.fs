module internal CodeAnalysis.Evaluator

open CodeAnalysis.Binding

let rec evaluateExpression (es: BoundExpression): obj =
    match es with
    | BoundBinaryExpression (left,op,right) ->
        let left = evaluateExpression left
        let right = evaluateExpression right
        let i (x: obj) = x :?> int
        let b (x: obj) = x :?> bool
        match op.Kind with
        | Addition -> i left + i right :> obj
        | Substraction -> i left - i right :> obj
        | Multiplication -> i left * i right :> obj
        | Division -> i left / i right :> obj
        | LogicalAnd -> (b left && b right) :> obj
        | LogicalOr -> (b left || b right) :> obj
        | Equals -> obj.Equals(left, right) :> obj
        | NotEquals -> not (obj.Equals(left, right)) :> obj
    | BoundLiteralExpression value ->
        if isNull value then
            0 :> obj
        else value
    | BoundUnaryExpression (op,exp) ->
        let operand = evaluateExpression exp
        match op.Kind with
        | Identity -> operand
        | Negation -> -(operand :?> int) :> obj
        | LogicalNegation -> operand :?> bool |> not :> obj
(*
let evaluate (se: SyntaxElement) =
    match se with
    | SyntaxNodeElement (n:SyntaxNode) ->
        match n with
        | ExpressionSyntax (es:ExpressionSyntax) -> evaluateExpression es
    | ExpressionSyntaxElement es -> evaluateExpression es
    | SyntaxTokenElement token -> failwith "cant even :'("
*)