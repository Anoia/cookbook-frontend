module Cookbook exposing (..)

import Browser
import Bulma.Elements exposing (TableRow, TitleSize(..), table, tableBody, tableCell, tableCellHead, tableFoot, tableHead, tableModifiers, tableRow, title)
import Bulma.Layout exposing (SectionSpacing(..), container, hero, heroBody, heroModifiers, section)
import Bulma.Modifiers exposing (Color(..))
import Html exposing (Html, div, text)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, string)



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias RecipeOverview =
    { id : Int, name : String, description : String }


type Model
    = Loading
    | Failure String
    | ViewAllRecipes (List RecipeOverview)


recipeOverviewDecoder : Decoder RecipeOverview
recipeOverviewDecoder =
    map3 RecipeOverview
        (field "id" int)
        (field "name" string)
        (field "description" string)


recipeListDecoder : Decoder (List RecipeOverview)
recipeListDecoder =
    list recipeOverviewDecoder


init : Int -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://localhost:8080/recipes"
        , expect = Http.expectJson LoadedAllRecipes recipeListDecoder
        }
    )



-- Update


type Msg
    = LoadedAllRecipes (Result Http.Error (List RecipeOverview))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadedAllRecipes (Ok value) ->
            ViewAllRecipes value |> noCmd

        LoadedAllRecipes (Err error) ->
            Failure (httpErrorString error) |> noCmd


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )



-- VIEW


createRecipeRow : RecipeOverview -> TableRow msg
createRecipeRow recipe =
    tableRow False
        []
        [ tableCell [] [ text recipe.name ]
        , tableCell [] [ text recipe.description ]
        ]


recipeTable : List RecipeOverview -> Bulma.Elements.Table msg
recipeTable recipes =
    table { tableModifiers | striped = True }
        []
        [ tableHead []
            [ tableRow False
                []
                [ tableCellHead [] [ text "Name" ]
                , tableCellHead [] [ text "Description" ]
                ]
            ]
        , tableBody [] (List.map createRecipeRow recipes)
        , tableFoot [] []
        ]


recipeHeader =
    hero { heroModifiers | color = Primary }
        []
        [ heroBody []
            [ container []
                [ title H1 [] [ text "CookBook" ]
                , title H2 [] [ text "Hero Subtitle" ]
                ]
            ]
        ]


recipeOverview : List RecipeOverview -> Html Msg
recipeOverview recipes =
    container []
        [ section NotSpaced
            []
            [ recipeHeader ]
        , section NotSpaced
            []
            [ recipeTable recipes ]
        ]


view : Model -> Html Msg
view model =
    case model of
        Loading ->
            section NotSpaced
                []
                [ container [] [ text "loading.." ] ]

        ViewAllRecipes recipes ->
            recipeOverview recipes

        Failure e ->
            div [] [ text "failed: ", text e ]


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl string ->
            "bad url: " ++ string

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus int ->
            "Bad status: " ++ String.fromInt int

        Http.BadBody string ->
            "Bad body: " ++ string



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
