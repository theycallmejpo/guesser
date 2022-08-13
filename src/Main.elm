module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, input, li, p, text, ul)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Set exposing (Set)


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { guesses : Set String
    , game : Game
    }


type alias Game =
    { name : String
    , list : Set String
    }


initialModel : Model
initialModel =
    { guesses = Set.empty
    , game =
        { name = "US States"
        , list =
            Set.fromList
                [ "Alabama"
                , "Alaska"
                , "Arizona"
                , "Arkansas"
                , "California"
                , "Colorado"
                , "Connecticut"
                , "Delaware"
                , "Florida"
                , "Georgia"
                , "Hawaii"
                , "Idaho"
                , "Illinois"
                , "Indiana"
                , "Iowa"
                , "Kansas"
                , "Kentucky"
                , "Louisiana"
                , "Maine"
                , "Maryland"
                , "Massachusetts"
                , "Michigan"
                , "Minnesota"
                , "Mississippi"
                , "Missouri"
                , "Montana"
                , "Nebraska"
                , "Nevada"
                , "New Hampshire"
                , "New Jersey"
                , "New Mexico"
                , "New York"
                , "North Carolina"
                , "North Dakota"
                , "Ohio"
                , "Oklahoma"
                , "Oregon"
                , "Pennsylvania"
                , "Rhode Island"
                , "South Carolina"
                , "South Dakota"
                , "Tennessee"
                , "Texas"
                , "Utah"
                , "Vermont"
                , "Virginia"
                , "Washington"
                , "West Virginia"
                , "Wisconsin"
                , "Wyoming"
                ]
        }
    }



-- UPDATE


type Msg
    = ItemSelected String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemSelected item ->
            let
                newGuesses =
                    Set.insert item model.guesses
            in
            { model | guesses = newGuesses }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "w-full p-8 lg:w-1/2 md:mx-auto" ]
        [ h1 [ class "font-bold text-2xl uppercase mb-4 text-gray-700" ] [ text model.game.name ]
        , viewSummary model model.guesses
        , viewItems
            (Set.toList model.game.list)
            (\i -> Set.member i model.guesses)
        ]


viewSummary : Model -> Set String -> Html msg
viewSummary model members =
    div [ class "flex flex-row flex-wrap mb-4 gap-2" ]
        [ div
            [ class "flex flex-row flex-nowrap gap-1 items-center" ]
            [ circle "green"
            , text (String.fromInt (Set.size members) ++ " correct")
            ]
        , div
            [ class "flex flex-row flex-nowrap gap-1 items-center" ]
            [ circle "gray"
            , text (String.fromInt (Set.size model.game.list - Set.size members) ++ " left")
            ]
        ]


circle : String -> Html msg
circle color =
    let
        makeCircle twClasses =
            div [ class ("w-2 h-2 rounded-full " ++ twClasses) ] []
    in
    case color of
        "green" ->
            makeCircle "bg-green-500"

        "gray" ->
            makeCircle "bg-gray-400"

        _ ->
            text ""


viewItem : (String -> Bool) -> String -> Html Msg
viewItem isItemSelected item =
    li
        [ classList
            [ ( "rounded-full border text-base px-3 py-1.5 cursor-pointer", True )
            , ( "border-gray-300 bg-gray-100 text-gray-900 hover:bg-gray-200", not (isItemSelected item) )
            , ( "border-green-500 bg-green-100 text-green-900", isItemSelected item )
            ]
        , onClick (ItemSelected item)
        ]
        [ text item ]


viewItems : List String -> (String -> Bool) -> Html Msg
viewItems list isItemSelected =
    ul
        [ class "flex flex-row flex-wrap gap-3" ]
        (List.map (viewItem isItemSelected) list)
