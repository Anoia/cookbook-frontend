module FoodStuff exposing (..)

import ApiHelper
import Bulma.Elements exposing (table, tableBody, tableCell, tableCellHead, tableFoot, tableHead, tableModifiers, tableRow)
import Html exposing (Html, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, field, int, list, map2, map3, string)


type alias FoodStuffOverview =
    { id : Int, name : String }


type alias Ingredient =
    { id : Int, name : String, amount : Int }


foodStuffDecoder : Decoder FoodStuffOverview
foodStuffDecoder =
    map2 FoodStuffOverview
        (field "id" int)
        (field "name" string)


foodStuffListDecoder : Decoder (List FoodStuffOverview)
foodStuffListDecoder =
    list foodStuffDecoder


ingredientDecoder : Decoder Ingredient
ingredientDecoder =
    map3 Ingredient
        (field "foodStuffId" int)
        (field "name" string)
        (field "amount" int)


ingredientListDecoder : Decoder (List Ingredient)
ingredientListDecoder =
    list ingredientDecoder


type Model
    = Loading
    | Failed String
    | ViewAll (List FoodStuffOverview)
    | ViewSingle FoodStuffOverview
    | CreateNew String


type Msg
    = CompletedLoadFoodstuffList (Result Http.Error (List FoodStuffOverview))
    | CompletedLoadFoodstuff
    | ClickedCreateNew
    | FoodStuffSelected Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompletedLoadFoodstuffList (Ok value) ->
            ( ViewAll value, Cmd.none )

        CompletedLoadFoodstuffList (Err e) ->
            ( Failed (ApiHelper.httpErrorString e), Cmd.none )

        CompletedLoadFoodstuff ->
            ( model, Cmd.none )

        ClickedCreateNew ->
            ( model, Cmd.none )

        FoodStuffSelected int ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model of
        ViewAll foodStuffOverviews ->
            foodStuffOverviewTable foodStuffOverviews

        ViewSingle foodStuffOverview ->
            text "single"

        CreateNew string ->
            text "create"

        Loading ->
            text "loading"

        Failed string ->
            text string


foodStuffOverviewTableRow : FoodStuffOverview -> Html Msg
foodStuffOverviewTableRow foodStuff =
    tableRow False
        [ onClick (FoodStuffSelected foodStuff.id) ]
        [ tableCell [] [ text (String.fromInt foodStuff.id) ]
        , tableCell [] [ text foodStuff.name ]
        ]


foodStuffOverviewTable : List FoodStuffOverview -> Html Msg
foodStuffOverviewTable foodStuffOverviews =
    table { tableModifiers | striped = True }
        []
        [ tableHead []
            [ tableRow False
                []
                [ tableCellHead [] [ text "ID" ]
                , tableCellHead [] [ text "Name" ]
                ]
            ]
        , tableBody [] (List.map foodStuffOverviewTableRow foodStuffOverviews)
        , tableFoot [] []
        ]
