namespace CodeAnalysis.Binding
open CodeAnalysis.Syntax.Syntax

type BoundUnaryOperator
    ( syntaxKind: SyntaxKind
    , kind: BoundUnaryOperatorKind
    , operandType: System.Type
    , resultType: System.Type) =

    new (syntaxKind: SyntaxKind, kind: BoundUnaryOperatorKind, operandType: System.Type) =
        BoundUnaryOperator(syntaxKind, kind, operandType, operandType)

    member __.SyntaxKind = syntaxKind
    member __.Kind = kind
    member __.OperandType = operandType
    member __.Type = resultType


module BoundUnaryOperator =
    let private operators =
        [
            BoundUnaryOperator(BangToken, LogicalNegation, typeof<bool>)
            BoundUnaryOperator(MinusToken, Negation, typeof<int>)
            BoundUnaryOperator(PlusToken, Identity, typeof<int>)
        ]
    let bind (syntaxKind: SyntaxKind) (operandType: System.Type) =
        operators
        |> List.tryFind (fun op -> op.SyntaxKind = syntaxKind && op.OperandType = operandType)