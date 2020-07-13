module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open Shared


// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type Model = { InputText: string; Results: string array Option; WordLength: int option; WordStart: string option }

// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type Msg =
    | Updated of string
    | InitialTextLoaded
    | SearchRequested
    | ResultsReady of string array
    | SetWordLength of string option
    | SetWordStart of string option

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =
    let initialModel = { InputText = ""; Results = None; WordLength = None; WordStart = None }
    let loadCountCmd =
        Cmd.ofMsg InitialTextLoaded
    initialModel, loadCountCmd

let getSubWords text : Fable.Core.JS.Promise<string array> =
    let url = sprintf "/api/search/%s" text
    Fetch.get(url)

// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match currentModel.InputText, msg with
    | _, Updated updatedText ->
        let nextModel = { currentModel with InputText = updatedText }
        nextModel, Cmd.none
    | _, InitialTextLoaded ->
        currentModel, Cmd.none
    | text, SearchRequested ->
        let search = Cmd.OfPromise.perform getSubWords text ResultsReady
        let nextModel = { currentModel with InputText = "Searching..." }
        nextModel, search
    | _, ResultsReady results ->
        let nextModel = { currentModel with Results = Some results }
        nextModel, Cmd.none
    | _, SetWordLength wordLength ->
        let nextWordLength =
            match wordLength with
            | None -> None
            | Some l -> Some (int l)

        let nextModel = { currentModel with WordLength = nextWordLength }
        nextModel, Cmd.none
    | _, SetWordStart wordStart ->
        let nextModel = { currentModel with WordStart = wordStart }
        nextModel, Cmd.none

let show = function
    | { InputText = text } -> text

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick
        ]
        [ str txt ]

let resultToTile result =
    Tile.tile [ Tile.IsChild ] [ Box.box' [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str result ] ]

let getTileSize divisor =
    match divisor with
    | 1 -> Tile.Is12 
    | 2 -> Tile.Is6
    | 3 -> Tile.Is4
    | 4 -> Tile.Is3
    | 6 -> Tile.Is2
    | _ -> Tile.Is1

let getSplitCollection divisor remainder (collection:string array) =
    [ for index in [0..collection.Length] do if index % divisor = remainder then Some collection.[index] else None ]
    |> List.choose id
    |> List.map resultToTile
    |> Tile.tile [ Tile.IsParent; Tile.IsVertical; Tile.Size (getTileSize divisor) ]

let splitByDivisor divisor (collection:string array) =
    let remainders = [0..divisor-1]
    [ for remainder in remainders do getSplitCollection divisor remainder collection ]

let filterByWordLength wordLength (results:string array) =
    match wordLength with
    | None -> results
    | Some filter ->
        Array.where (fun word -> word.Length = filter) results

let filterByWordStart wordStart (results:string array) =
    match wordStart with
    | None -> results
    | Some filter ->
        Array.where (fun word -> word.StartsWith(filter)) results

let subwordTiles = function
    | { Results = None } -> []
    | { Results = Some results; WordLength = wordLength; WordStart = wordStart } ->
        results
        |> filterByWordLength wordLength
        |> filterByWordStart wordStart
        |> splitByDivisor 3 

let input onChange =
    Input.search [ Input.OnChange onChange ]

let handleOptionValueChange message value dispatch  =
    if value <> "" then dispatch (message (Some value)) else dispatch (message None)

let wordLengthFilter dispatch =
    Field.div []
        [ Control.div [ Control.IsExpanded ]
            [ Input.number [ Input.Placeholder "Length"; Input.OnChange (fun event -> handleOptionValueChange SetWordLength event.Value dispatch) ] ]
        ]

let wordStartFilter dispatch =
    Field.div []
        [ Control.div [Control.IsExpanded]
            [Input.text [ Input.Placeholder "Starts With"; Input.OnChange (fun event -> handleOptionValueChange SetWordStart event.Value dispatch) ]]
        ]

let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
                [ Navbar.Item.div [ ]
                    [ Heading.h2 [ ]
                        [ str "SubWord" ] ] ]

          Section.section []
            [ Container.container [ Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ Field.div [ Field.HasAddons; Field.HasAddonsCentered; Field.HasAddonsFullWidth ] [
                        Control.div [] [ input (fun event -> dispatch (Updated event.Value)) ]
                        Control.div [] [ button "Search" (fun _ -> dispatch SearchRequested ) ] ]
                ]
            ]

          Section.section []
            [ Container.container []
                [ Field.div [Field.IsHorizontal]
                    [ wordStartFilter dispatch; wordLengthFilter dispatch; ]
                ]
            ]

          Section.section [ Section.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ]  ]
            [ Container.container []
                [ Tile.ancestor [ ] (subwordTiles model) ]
            ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
