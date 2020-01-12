module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

// MODEL

type Card =
    { text: string; id: int }

let init_cards = [ { text = "first card"; id = 1 }
                   { text = "second card"; id = 2 } ]

type Model = Card list

type Msg =
| Add

let init() : Model = init_cards

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | Add -> model @ [{ text = "a new todo item!"; id = model.Length }]
    //| Delete -> model - 1

// VIEW (rendered with React)

let view (model:Model) dispatch =

  div []
      [ ol
          []
          (model
           |> List.map (fun i -> li [] [ str i.text ]))
        button [ OnClick (fun _ -> dispatch Add) ] [ str "+" ] ]

// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
