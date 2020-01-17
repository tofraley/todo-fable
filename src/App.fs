module App

open Fable.Core
open Fable.React
open Fable.React.Props
open Browser.Types
open Browser
open Elmish
open Elmish.React

let [<Literal>] ENTER_KEY = 13.

// MODEL

type Card =
    { text: string
      id: int }

type Model = { entries: Card list
               field: string }

type Msg =
| Add
| Delete of int
| UpdateField of string

let init_cards = { entries = []
                   field = "" }

let init() : Model = init_cards

let newEntry text id =
    { text = text 
      id = id }
// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Add ->
        let xs = if System.String.IsNullOrEmpty model.field then
                    model.entries
                 else
                    model.entries @ [ newEntry model.field model.entries.Length ]
        { model with
            field = ""
            entries = xs }

    | UpdateField str -> { model with field = str }
    | Delete id -> { model with entries = List.filter (fun t -> t.id <> id) model.entries }

// VIEW (rendered with React)

let onEnter (msg:Msg) dispatch =
    OnKeyDown (fun ev ->
        if ev.keyCode = ENTER_KEY then
            dispatch msg)

let targetValue (ev: Event) =
    (ev.target :?> HTMLInputElement).value

let viewInput (model:string) dispatch =
    input [
        Class "new-todo"
        Placeholder "What needs to be done?"
        Value model
        onEnter Add dispatch
        OnChange (fun ev ->
            targetValue ev |> UpdateField |> dispatch)
        AutoFocus true
    ]

let view (model:Model) dispatch =
  div []
      [ ol
          []
          (model.entries
           |> List.map (fun i -> li []
                                    [ str i.text
                                      button [ OnClick (fun _ -> Delete i.id |> dispatch) ]
                                             [ str "-" ]
                                    ]))
        viewInput model.field dispatch
      ]

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
