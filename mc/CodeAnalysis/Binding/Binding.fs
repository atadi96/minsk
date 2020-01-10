namespace CodeAnalysis.Binding

type internal BoundNodeKind =
    | LiteralExpression
    | UnaryExpression
    | BinaryExpression

type internal IBoundNode =
    abstract member Kind: BoundNodeKind

type BoundUnaryOperatorKind =
    | Identity
    | Negation

type BoundBinaryOperatorKind =
    | Addition
    | Substraction
    | Multiplication
    | Division

type BoundExpression =
    | BoundLiteralExpression of obj
    | BoundUnaryExpression of BoundUnaryOperatorKind * BoundExpression
    | BoundBinaryExpression of BoundExpression * BoundBinaryOperatorKind * BoundExpression

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
