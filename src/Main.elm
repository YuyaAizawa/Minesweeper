import Browser
import List exposing (..)
import Maybe exposing (withDefault)
import Random exposing (initialSeed, step)
import Random.List exposing (shuffle)
import Html exposing (Html, table, tbody, tr, td, input, div, text)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onClick, onInput)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { field : List (List FieldStatus)
  , nextSeed : Maybe Int
  }

type alias FieldStatus =
  { opened : Bool
  , underground : Underground
  }

type Underground
  = Mine
  | Empty Int

init : Model
init =
  let width = 8 in
  let height = 12 in
  let mines = 40 in

  { field = newField width height mines (initialSeed 0)
  , nextSeed =  Nothing
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
          then FieldStatus False Mine
          else
            let
              num = neighbor (x, y)
                |> map (\c -> if member c minedCoords then 1 else 0)
                |> sum
            in
            FieldStatus False (Empty num)
        )
      )

-- UPDATE

type Msg
  = Reset
  | Open Coords
  | Seed String

type alias Coords = (Int, Int)

update : Msg -> Model -> Model
update msg model =
  let _ = Debug.log "msg" <| msgToString msg in
  case msg of
    Reset -> case model.nextSeed of
      Nothing -> model
      Just seed ->
        let width = 8 in
        let height = 12 in
        let mines = 40 in
        { model | field =
          newField width height mines (initialSeed seed)
        }

    Open coords ->
      { model | field =
        model.field
          |> indexed2Map (\x -> \y -> \u -> if coords == (x, y)
            then {opened = True, underground = u.underground}
            else u)
      }

    Seed seed -> { model | nextSeed = seed |> String.toInt }

msgToString : Msg -> String
msgToString msg = case msg of
  Reset -> "Reset"
  Open coords -> coords |> coordsToString
  Seed str -> "Input " ++ str

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
    ]

mineField : Model -> List (Html Msg)
mineField model =
  model.field
    |> indexed2Map (\x -> \y -> \f -> td [][coordButton (x, y) f])
    |> map (tr [])

coordButton : Coords -> FieldStatus -> Html Msg
coordButton coords field =
  let
    txt = case (field.opened, field.underground) of
      (True, Mine) -> "\u{1F4A3}"
      (True, Empty i) -> i |> String.fromInt
      (False, _) -> coordsToString coords
  in
    input [
      type_ "button",
      onClick <| Open coords,
      disabled field.opened,
      value txt][]