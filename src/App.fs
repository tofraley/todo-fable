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
               field: string
               name: string
               isEditing: bool }

type Msg =
| Add
| Delete of int
| UpdateField of string
| EditingName
| StopEditingName
| UpdateName of string

let init_cards = { entries = []
                   field = "What needs to be done?"
                   name = "new list"
                   isEditing = false }

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
    | EditingName -> { model with isEditing = true }
    | StopEditingName -> { model with isEditing = false }
    | UpdateName str -> { model with name = str }

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
        Value model
        onEnter Add dispatch
        OnChange (fun ev ->
            targetValue ev |> UpdateField |> dispatch)
        AutoFocus true
    ]

let nameInput (model:string) dispatch =
    input [
        Class "new-todo"
        Value model
        onEnter StopEditingName dispatch
        OnBlur (fun _ -> StopEditingName |> dispatch)
        OnChange (fun ev ->
            targetValue ev |> UpdateName |> dispatch)
        AutoFocus true
    ]

let clickable_h1 dispatch =
    h1 [ OnClick (fun _ -> EditingName |> dispatch) ]

let listName isEditing (name:string) dispatch =
  if isEditing then
      nameInput name dispatch
  else
      (clickable_h1 dispatch [ str name ])

let view (model:Model) dispatch =
  div []
      [
        listName model.isEditing model.name dispatch
        ol
          []
          (model.entries
          |> List.map (fun i ->
                            li
                              []
                              [ str i.text
                                button [ OnClick (fun _ -> Delete i.id |> dispatch) ]
                                        [ str "X" ] ]))
        viewInput model.field dispatch
      ]

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
