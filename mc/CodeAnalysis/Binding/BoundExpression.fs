namespace CodeAnalysis.Binding

type BoundExpression =
    | BoundLiteralExpression of obj
    | BoundUnaryExpression of BoundUnaryOperator * BoundExpression
    | BoundBinaryExpression of BoundExpression * BoundBinaryOperator * BoundExpression

    interface IBoundNode with
        member this.Kind =
            match this with
            | BoundLiteralExpression _ -> LiteralExpression
            | BoundUnaryExpression _ -> UnaryExpression
            | BoundBinaryExpression _ -> BinaryExpression
    member this.Type =
        match this with
        | BoundLiteralExpression value -> value.GetType()
        | BoundUnaryExpression (_,be) -> be.Type
        | BoundBinaryExpression (l,_,_) -> l.Type
