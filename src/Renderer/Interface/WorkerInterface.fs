module WorkerInterface


open Browser.Types
open Browser.Blob
open Browser.Dom
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json
open Fable.React
open Fable.React.Props


[<Global>]
let URL : obj = jsNative

[<Emit("self.postMessage($0)")>]
let postMessage a : unit = jsNative


[<Emit("onmessage = ($0)")>]
let defineWorkerOnMsg onMsgFn = jsNative


[<Emit("$1.postMessage($0)")>]
let sendWorkerMsg msg worker = jsNative

[<Emit("$0.terminate()")>]
let terminateWorker worker = jsNative

[<Emit("close()")>]
let closeWorker() = jsNative

[<Emit("new Worker(new URL($0, import.meta.url), {type: \"module\"})")>]
// [<Emit("new Worker($0)")>]
let inline newWorkerUrl url = jsNative

[<Emit("$1.onmessage = ($0)")>]
let inline setWorkerOnMsg onMsgFun worker = jsNative

let unpackJsonDecode =
    function
    | Ok value -> value
    | Error error -> failwithf "Could not decode Json: %A" error

let encoderIprop (input: IProp) =
    Encode.string (Encode.toString 0 input)


let decoderWaveIprop : Decoder<IProp> =
    Decode.oneOf [
        Decode.Auto.generateDecoder<SVGAttr>()
        |> Decode.map (fun someSVGAttr -> someSVGAttr :> IProp)

        Decode.tuple2 Decode.string (Decode.object (fun get ->
                [
                    get.Required.Field "display" (
                        Decode.Auto.generateDecoder<DisplayOptions>()
                        |> Decode.andThen (fun displayOptions -> Decode.succeed (Display displayOptions))
                    )
                    get.Required.Field "borderBottom" (
                        Decode.Auto.generateDecoder<obj>()
                        |> Decode.andThen (fun o -> Decode.succeed (BorderBottom o))
                    )
                ]
            ))
        |> Decode.andThen (fun (style, cssOpts) ->
            match style, cssOpts with
            | "style", opts -> Decode.succeed (Style opts)
            | _ , _ -> Decode.fail "Expected 'style' field")
    ]

let decoderPolyline : Decoder<ReactElement> =
    Decode.object (fun get ->
        polyline
            (get.Required.Field "props" (Decode.object (fun get ->
                [
                    SVGAttr.Stroke (get.Required.Field "stroke" Decode.string)
                    SVGAttr.Fill (get.Required.Field "fill" Decode.string)
                    SVGAttr.StrokeWidth (get.Required.Field "strokeWidth" (Decode.Auto.generateDecoder<obj>()))
                    Points (get.Required.Field "points" Decode.string)
                ])))
            []
    )



let decoderText : Decoder<ReactElement> =
    Decode.object (fun get ->
        get.Required.Field "props" (Decode.object (fun get ->
            text
                [
                    get.Required.Field "x" (
                        Decode.float
                        |> Decode.andThen (fun x -> Decode.succeed (X x))
                    )
                    get.Required.Field "y" (
                        Decode.float
                        |> Decode.andThen (fun y -> Decode.succeed (Y y))
                    )
                    Style [
                        get.Required.Field "style" (
                            Decode.object (fun get ->
                                get.Required.Field "fontSize" (
                                    Decode.Auto.generateDecoder<obj>()
                                    |> Decode.andThen (fun o -> Decode.succeed (FontSize o))
                        )))
                    ]
                ]
                [
                    get.Required.Field "children" (
                        Decode.string
                        |> Decode.andThen (fun s -> Decode.succeed (str s))
                    )
                ]
    )))

let decoderPolylineOrText : Decoder<ReactElement> =
    Decode.field "type" Decode.string
    |> Decode.andThen (function
        | "text" -> decoderText
        | "polyline" -> decoderPolyline
        | _ -> Decode.fail "this decoder works only for text and polyline react elements")

let encoderReactElement (input: ReactElement) =
    Encode.string (Encode.toString 0 input)

let decoderReactElement' : Decoder<ReactElement> =
    Decode.field "type" Decode.string
    |> Decode.andThen (function
        | "text" -> decoderText
        | "svg" ->
            Decode.object (fun get ->
                    get.Required.Field "props" (
                        Decode.object (fun get ->
                            svg
                                [
                                    SVGAttr.Height (get.Required.Field "height" (Decode.Auto.generateDecoder<obj>()))
                                    SVGAttr.Width (get.Required.Field "width" (Decode.Auto.generateDecoder<obj>()))
                                    SVGAttr.ViewBox (get.Required.Field "viewBox" Decode.string)
                                    SVGAttr.PreserveAspectRatio (get.Required.Field "preserveAspectRatio" Decode.string)
                                    Style
                                        (get.Required.Field "style" (
                                            (Decode.object (fun get ->
                                            [
                                                get.Required.Field "display" (
                                                    Decode.Auto.generateDecoder<DisplayOptions>()
                                                    |> Decode.andThen (fun displayOptions -> Decode.succeed (Display displayOptions))
                                                )
                                                get.Required.Field "borderBottom" (
                                                    Decode.Auto.generateDecoder<obj>()
                                                    |> Decode.andThen (fun o -> Decode.succeed (BorderBottom o))
                                                )
                                            ]
                                        ))))
                                ]

                                (get.Required.Field "children" (Decode.oneOf [
                                    Decode.list decoderPolylineOrText
                                    decoderPolyline |> Decode.andThen (fun poly -> Decode.succeed [poly])
                                    ]))
                        )
                    )
            )
        | _ -> Decode.fail "unexpected react element")

let decoderReactElement : Decoder<ReactElement> =
    Decode.string
    |> Decode.andThen (fun s ->
        Decode.fromString decoderReactElement' s
        |> function
            | Ok someReactElement -> Decode.succeed someReactElement
            | Error err -> Decode.fail err)