module CodeAnalysis.Binding.Diagnostics

type DiagnosticsValue = string
type Diagnostics<'T> = private Diagnostics of (DiagnosticsValue seq -> 'T * DiagnosticsValue seq)

module Diagnostics =
    let ret x = Diagnostics (fun ds -> x, ds)
    let retFrom x = x
    let map (f: 'a -> 'b) (Diagnostics d: Diagnostics<'a>) =
        Diagnostics (fun ds ->
            let (v,ds) = d ds
            f v, ds
        )
    let bind f (Diagnostics d) =
        Diagnostics (fun ds ->
            let (x,ds') = d ds
            let (Diagnostics d') = f x
            d' ds'
        )

    let run (initialDiags: DiagnosticsValue seq) (Diagnostics ds) =
        let (x,ds) = ds initialDiags
        x, ds |> Seq.toArray

type DiagnosticsBuilder() =
    member __.Bind(x,f) = Diagnostics.bind f x
    member __.Return x = Diagnostics.ret x

let diagnose (err: DiagnosticsValue)= Diagnostics (fun ds -> (), Seq.append ds [err])
let diagnostics = DiagnosticsBuilder()