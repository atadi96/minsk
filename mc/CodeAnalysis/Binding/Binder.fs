module CodeAnalysis.Binding.Binder

open System
open CodeAnalysis.Syntax.Syntax
open Diagnostics

let rec bindExpression (syntax: ExpressionSyntax): Diagnostics<BoundExpression> =
    match syntax with
    | LiteralExpression (_,value) ->
        diagnostics { return BoundLiteralExpression value }
    | UnaryExpression (opToken,exp) ->
        diagnostics {
            let! boundExp = bindExpression exp
            let typ = boundExp.Type
            match BoundUnaryOperator.bind opToken.Kind typ with
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
            match BoundBinaryOperator.bind opToken.Kind lType rType with
            | Some boundBinaryOperator ->
                    return BoundBinaryExpression(boundLeft,boundBinaryOperator,boundRight)
            | None ->
                do! diagnose (sprintf "Binary operator '%s' is not defined for types '%A' and '%A'." opToken.Text lType rType)
                return boundLeft
        }
    | ParenthesizedExpression (_,exp,_) -> bindExpression exp

let bind (se: SyntaxElement) =
    match se with
    | ExpressionSyntaxElement ese -> bindExpression ese
    | _ -> failwith "not implemented"