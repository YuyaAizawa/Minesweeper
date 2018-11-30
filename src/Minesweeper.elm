module Minesweeper exposing (..)

import Browser
import List exposing (..)
import Maybe exposing (withDefault)
import Random exposing (initialSeed, step)
import Random.List
import Html exposing (Html, table, tbody, tr, td, input, div, text, fieldset, label)
import Html.Attributes exposing (disabled, type_, value, name)
import Html.Events exposing (onClick, onInput)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { field : Field
  , nextSeed : Maybe Int
  , action : Action
  }

type alias Field = List (List Cell)

type alias Cell =
  { surface : Surface
  , underground : Underground
  }

type Surface
  = Covered
  | Uncovered
  | Flagged

type Underground
  = Mine
  | Empty Int

type alias Coords = { x : Int, y : Int }

type alias Difficulty =
  { width : Int
  , height : Int
  , mines : Int
  }

defalutDifficulty =
  { width = 8
  , height = 12
  , mines = 30
  }

init : Model
init =
  { field = newField defalutDifficulty (initialSeed 0)
  , nextSeed =  Nothing
  , action = Dig
  }

newField : Difficulty -> Random.Seed -> Field
newField difficulty seed =

  let
    allCoords : List (List Coords)
    allCoords =
      range 0 (difficulty.height - 1)
      |> map (\y -> range 0 (difficulty.width - 1)
        |> map (\x -> Coords x y))
  in

  let
    minedCoords : List Coords
    minedCoords =
      allCoords
        |> concat
        |> Random.List.shuffle
        |> (\l -> step l seed)
        |> Tuple.first
        |> take difficulty.mines
  in

  let
    neighbor : Coords -> List Coords
    neighbor c =
      let x = c.x in
      let y = c.y in
      [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
      ,(x - 1, y    ), (x, y    ), (x + 1, y    )
      ,(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
        |> map (\n -> { x = Tuple.first n, y = Tuple.second n })
  in

  allCoords
    |> indexed2Map (\c -> \_ ->
      if member c minedCoords
        then
          Cell Covered Mine
        else
          let
            num = neighbor c
              |> map (\n -> if member n minedCoords then 1 else 0)
              |> sum
          in
          Cell Covered (Empty num)
    )


-- UPDATE

type Msg
  = Reset
  | Open Coords
  | Seed String
  | ChangeAction Action

type Action
  = Dig
  | Flag

update : Msg -> Model -> Model
update msg model =
  let
    updateField : Coords -> (Cell -> Cell) -> Field -> Field
    updateField coords updater =
      indexed2Map (\c -> \f ->
          if c == coords
            then updater f
            else f
      )
  in
  case msg of
    Reset -> case model.nextSeed of
      Nothing -> model
      Just seed ->
        { model | field =
          newField defalutDifficulty (initialSeed seed)
        }

    Open coords -> case model.action of
      Dig ->
        { model | field =
          model.field
            |> updateField coords (\f ->
              if f.surface == Covered
                then {f | surface = Uncovered}
                else f
            )
        }
      Flag ->
        { model | field =
          model.field
            |> updateField coords (\f ->
              case f.surface of
                Covered -> { f | surface = Flagged }
                Flagged -> { f | surface = Covered }
                Uncovered -> f
            )
        }

    Seed seed -> { model | nextSeed = seed |> String.toInt }

    ChangeAction action -> { model | action = action }

msgToString : Msg -> String
msgToString msg = case msg of
  Reset -> "Reset"
  Open coords -> coords |> coordsToString
  Seed str -> "Input " ++ str
  ChangeAction action -> "ChangeAction " ++ case action of
    Dig -> "Dig"
    Flag -> "Flag"

coordsToString : Coords -> String
coordsToString coords =
  "(" ++ String.fromInt coords.x ++ ", " ++ String.fromInt coords.y ++ ")"

indexed2Map : (Coords -> a -> b) -> List (List a) -> List (List b)
indexed2Map f =
  indexedMap (\y ->
    indexedMap (\x ->
      f (Coords x y)))


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div []
      [ input
        [ type_ "button"
        , onClick Reset
        , value "reset"
        ][]
      , input
        [ type_ "number"
        , onInput Seed
        ][]
      ]
    , table []
      [ tbody [] (mineField model)
      ]
    , viewPicker "clickTo"
      [ ("Dig" , ChangeAction Dig)
      , ("Flag", ChangeAction Flag)
      ]
    ]

mineField : Model -> List (Html Msg)
mineField model =
  model.field
    |> indexed2Map (\c -> \f ->
      td [][coordButton c f]
    )
    |> map (tr [])

coordButton : Coords -> Cell -> Html Msg
coordButton coords field =
  let
    txt = case field.surface of
      Covered -> "\u{3000}"
      Flagged -> "\u{1F6A9}"
      Uncovered -> case field.underground of
        Mine -> "\u{1F4A3}"
        Empty i -> i |> String.fromInt
  in
  input
  [ type_ "button"
  , onClick (Open coords)
  , disabled (field.surface == Uncovered)
  , value txt
  ][]

viewPicker : String -> List (String, Msg) -> Html Msg
viewPicker group options =
  fieldset [] (options |> map (radio group))

radio : String -> (String, Msg) -> Html Msg
radio group (string, msg) =
  label []
    [ input
      [ type_ "radio"
      , name group
      , onClick msg
      ][]
    , text string
    ]