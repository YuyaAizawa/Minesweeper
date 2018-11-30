module Minesweeper exposing (..)

import Browser
import List exposing (..)
import Maybe exposing (withDefault)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Html exposing (Html, table, tbody, tr, td, input, div, text, fieldset, label)
import Html.Attributes exposing (disabled, type_, value, name)
import Html.Events exposing (onClick, onInput)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { field : List (List FieldStatus)
  , nextSeed : Maybe Int
  , clickTo : ClickTo
  }

type alias FieldStatus =
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

init : Model
init =
  let width = 8 in
  let height = 12 in
  let mines = 30 in

  { field = newField width height mines (initialSeed 0)
  , nextSeed =  Nothing
  , clickTo = Dig
  }

newField : Int -> Int -> Int -> Random.Seed -> List (List FieldStatus)
newField width height mines seed =
  let
    coords =
      range 0 (height - 1)
      |> map (\y -> range 0 (width - 1)
        |> map (\x -> (x, y)))
      |> concat
  in

  let
    minedCoords =
      shuffle coords
        |> (\l -> step l seed)
        |> Tuple.first
        |> take mines
  in

  let
    neighbor : (Int, Int) -> List (Int, Int)
    neighbor (x, y) =
      [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1)
      ,(x - 1, y    ), (x, y    ), (x + 1, y    )
      ,(x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
  in

  range 0 (height - 1)
      |> map (\y -> range 0 (width - 1)
        |> map (\x ->
          if member (x, y) minedCoords
          then FieldStatus Covered Mine
          else
            let
              num = neighbor (x, y)
                |> map (\c -> if member c minedCoords then 1 else 0)
                |> sum
            in
            FieldStatus Covered (Empty num)
        )
      )

-- UPDATE

type Msg
  = Reset
  | Open Coords
  | Seed String
  | ClickMode ClickTo

type alias Coords = (Int, Int)

type ClickTo
  = Dig
  | Flag

update : Msg -> Model -> Model
update msg model =
  let
    updateField coords updater field =
      field |> indexed2Map (\x -> \y -> \f -> if (coords == (x, y))
        then updater f
        else f)
  in
  case msg of
    Reset -> case model.nextSeed of
      Nothing -> model
      Just seed ->
        let width = 8 in
        let height = 12 in
        let mines = 30 in
        { model | field =
          newField width height mines (initialSeed seed)
        }

    Open coords -> case model.clickTo of
      Dig ->
        { model | field =
          model.field
            |> updateField coords (\f -> if f.surface == Covered
              then {f | surface = Uncovered}
              else f)
        }
      Flag ->
        { model | field =
          model.field
            |> updateField coords (\f -> case f.surface of
              Covered -> {f | surface = Flagged}
              Flagged -> {f | surface = Covered}
              Uncovered -> f)
        }

    Seed seed -> { model | nextSeed = seed |> String.toInt }

    ClickMode mode -> { model | clickTo = mode }

msgToString : Msg -> String
msgToString msg = case msg of
  Reset -> "Reset"
  Open coords -> coords |> coordsToString
  Seed str -> "Input " ++ str
  ClickMode mode -> "ClickTo " ++ case mode of
    Dig -> "Dig"
    Flag -> "Flag"

coordsToString : Coords -> String
coordsToString (x, y) =
  "(" ++ String.fromInt x ++ ", " ++ String.fromInt y ++ ")"

indexed2Map : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
indexed2Map f matrix =
  matrix
    |> indexedMap (\y -> \list -> list
      |> indexedMap (\x -> \a -> f x y a))


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
      [ tbody [] <| mineField model
      ]
    , viewPicker "clickTo"
      [ ("Dig" ,ClickMode Dig)
      , ("Flag", ClickMode Flag)
      ]
    ]

mineField : Model -> List (Html Msg)
mineField model =
  model.field
    |> indexed2Map (\x -> \y -> \f -> td [][coordButton (x, y) f])
    |> map (tr [])

coordButton : Coords -> FieldStatus -> Html Msg
coordButton coords field =
  let
    txt = case field.surface of
      Covered -> "\u{3000}"
      Flagged -> "\u{1F6A9}"
      Uncovered -> case field.underground of
        Mine -> "\u{1F4A3}"
        Empty i -> i |> String.fromInt
  in
  input [
    type_ "button",
    onClick <| Open coords,
    disabled <| field.surface == Uncovered,
    value txt][]

viewPicker : String -> List (String, Msg) -> Html Msg
viewPicker group options =
  fieldset [] (options |> map (radio group))

radio : String -> (String, Msg) -> Html Msg
radio group (string, msg) =
  label []
    [ input [ type_ "radio", name group, onClick msg ] []
    , text string
    ]