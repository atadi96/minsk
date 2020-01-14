namespace CodeAnalysis.Binding
open CodeAnalysis.Syntax.Syntax

type BoundBinaryOperator
    ( syntaxKind: SyntaxKind
    , kind: BoundBinaryOperatorKind
    , leftType: System.Type
    , rightType: System.Type
    , resultType: System.Type) =
    new (syntaxKind: SyntaxKind, kind: BoundBinaryOperatorKind, operandType: System.Type) =
        BoundBinaryOperator(syntaxKind, kind, operandType, operandType, operandType)

    member __.SyntaxKind = syntaxKind
    member __.Kind = kind
    member __.LeftType = leftType
    member __.RightType = rightType
    member __.ResultType = resultType

module BoundBinaryOperator =
    let private operators =
        [
            BoundBinaryOperator(PlusToken, Addition, typeof<int>)
            BoundBinaryOperator(MinusToken, Substraction, typeof<int>)
            BoundBinaryOperator(StarToken, Multiplication, typeof<int>)
            BoundBinaryOperator(SlashToken, Division, typeof<int>)
            BoundBinaryOperator(AmpersandAmpersandToken, LogicalAnd, typeof<bool>)
            BoundBinaryOperator(PipePipeToken, LogicalOr, typeof<bool>)
        ]
    let bind (syntaxKind: SyntaxKind) (leftType: System.Type) (rightType: System.Type)=
        operators
        |> List.tryFind (fun op ->
            op.SyntaxKind = syntaxKind &&
            op.LeftType = leftType &&
            op.RightType = rightType
        )