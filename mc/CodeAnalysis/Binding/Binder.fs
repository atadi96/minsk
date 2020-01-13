module CodeAnalysis.Binding.Binder

open System
open CodeAnalysis.Syntax.Syntax
open Diagnostics

let both<'T> lType rType = lType = typeof<'T> && rType = typeof<'T>

let bindUnaryOperator (kind: SyntaxKind) (typ: Type) =
    if typ = typeof<Int32> then
        match kind with
        | PlusToken -> Some Identity
        | MinusToken -> Some Negation
        | _ -> failwith (sprintf "Unexpected unary operator '%A'" kind)
    else if typ = typeof<bool> then
        match kind with
        | BangToken -> Some LogicalNegation
    else
        None

let bindBinaryOperator (kind: SyntaxKind) (lType:Type) (rType:Type) =
    if both<int32> lType rType then
        match kind with
        | PlusToken -> Some Addition
        | MinusToken -> Some Substraction
        | StarToken -> Some Multiplication
        | SlashToken -> Some Division
        | _ -> None
    else if both<bool> lType rType then
        match kind with
        | AmpersandAmpersandToken -> Some LogicalAnd
        | PipePipeToken -> Some LogicalOr
        | _ -> None
    else None

let rec bindExpression (syntax: ExpressionSyntax): Diagnostics<BoundExpression> =
    match syntax with
    | LiteralExpression (_,value) ->
        diagnostics { return BoundLiteralExpression value }
    | UnaryExpression (opToken,exp) ->
        diagnostics {
            let! boundExp = bindExpression exp
            let typ = boundExp.Type
            match bindUnaryOperator opToken.Kind typ with
            | Some boundUnaryOperator ->
                return BoundUnaryExpression (boundUnaryOperator,boundExp)
            | None ->
                do! diagnose (sprintf "Unary operator '%s' is not defined for type '%A'." opToken.Text typ)
                return boundExp
        }
    | BinaryExpression (left,opToken,right) ->
        diagnostics {
            let! boundLeft = bindExpression left
            let lType = boundLeft.Type
            let! boundRight = bindExpression right
            let rType = boundRight.Type
            match bindBinaryOperator opToken.Kind lType rType with
            | Some boundBinaryOperator ->
                    return BoundBinaryExpression(boundLeft,boundBinaryOperator,boundRight)
            | None ->
                do! diagnose (sprintf "Binary operator '%s' is not defined for types '%A' and '%A'." opToken.Text lType rType)
                return boundLeft
        }

let bind (se: SyntaxElement) =
    match se with
    | ExpressionSyntaxElement ese -> bindExpression ese
    | _ -> failwith "not implemented"