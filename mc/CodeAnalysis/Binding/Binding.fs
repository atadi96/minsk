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
    | LogicalNegation

type BoundBinaryOperatorKind =
    | Addition
    | Substraction
    | Multiplication
    | Division
    | LogicalAnd
    | LogicalOr
    | Equals
    | NotEquals
