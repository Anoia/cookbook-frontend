module Main exposing (..)

import Browser
import Html exposing (Html, b, button, div, h1, h3, input, label, li, p, text, ul)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type ViewStatus
    = ListRecipesView
    | SingleRecipeView Int Recipe
    | EditRecipeView ( Maybe Int, Recipe )


type alias Recipe =
    { name : String, ingredients : String, instructions : String }


type alias Model =
    { count : Int
    , status : ViewStatus
    , recipes : List Recipe
    }


init : Int -> ( Model, Cmd Msg )
init x =
    let
        initialRecipes =
            [ { name = "recipe1", ingredients = "ingr1, ingr2", instructions = "some instructions go here" }
            , { name = "recipe2", ingredients = "ingr5, ingr7", instructions = "some other instructions go here" }
            ]
    in
    ( { count = x, recipes = initialRecipes, status = ListRecipesView }, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement
    | ShowEditCreateRecipe (Maybe ( Int, Recipe ))
    | ShowList
    | ShowSingle Int Recipe
    | AddRecipe (Maybe Int) Recipe
    | EditRecipe (Maybe Int) Recipe RecipeUpdate


type RecipeUpdate
    = ChangedName String
    | ChangedIngredients String
    | ChangedInstructions String


getRecipeOrEmpty : Maybe ( Int, Recipe ) -> ( Maybe Int, Recipe )
getRecipeOrEmpty maybeRecipe =
    case maybeRecipe of
        Just a ->
            ( Just (Tuple.first a), Tuple.second a )

        Nothing ->
            ( Nothing, { name = "", ingredients = "", instructions = "" } )


noCmd : Model -> ( Model, Cmd Msg )
noCmd model =
    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateNoCmd msg model |> noCmd


updateNoCmd : Msg -> Model -> Model
updateNoCmd msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        ShowEditCreateRecipe maybeRecipe ->
            { model | status = EditRecipeView (getRecipeOrEmpty maybeRecipe) }

        ShowList ->
            { model | status = ListRecipesView }

        AddRecipe maybeId r ->
            let
                updatedRecipes =
                    Maybe.map (removeRecipeByIndex model.recipes) maybeId
                        |> Maybe.withDefault model.recipes
            in
            { model | recipes = r :: updatedRecipes, status = ListRecipesView }

        ShowSingle int recipe ->
            { model | status = SingleRecipeView int recipe }

        EditRecipe maybeId recipe newRecipeUpdated ->
            updateNewRecipe model maybeId recipe newRecipeUpdated


removeRecipeByIndex : List Recipe -> Int -> List Recipe
removeRecipeByIndex recipes index =
    List.filter (byIndex index) (List.indexedMap Tuple.pair recipes)
        |> List.map Tuple.second


byIndex : Int -> ( Int, Recipe ) -> Bool
byIndex index tuple =
    not (Tuple.first tuple == index)


updateNewRecipe : Model -> Maybe Int -> Recipe -> RecipeUpdate -> Model
updateNewRecipe model maybeId recipe recipeUpdate =
    { model | status = EditRecipeView ( maybeId, applyChangeToRecipe recipe recipeUpdate ) }


applyChangeToRecipe : Recipe -> RecipeUpdate -> Recipe
applyChangeToRecipe recipe recipeUpdate =
    case recipeUpdate of
        ChangedName string ->
            { recipe | name = string }

        ChangedIngredients string ->
            { recipe | ingredients = string }

        ChangedInstructions string ->
            { recipe | instructions = string }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Decrement ] [ text "minus" ]
            , div [] [ text (String.fromInt model.count) ]
            , button [ onClick Increment ] [ text "plus" ]
            ]
        , div [] [ currentRecipeView model ]
        ]


currentRecipeView : Model -> Html Msg
currentRecipeView model =
    case model.status of
        ListRecipesView ->
            div []
                [ createRecipesListView model.recipes
                , button [ onClick (ShowEditCreateRecipe Nothing) ] [ text "add new" ]
                ]

        SingleRecipeView id recipe ->
            div []
                [ h3 [] [ text recipe.name ]
                , b [] [ text "Ingredients" ]
                , p [] [ text recipe.ingredients ]
                , b [] [ text "Instructions" ]
                , p [] [ text recipe.instructions ]
                , button [ onClick ShowList ] [ text "back" ]
                , button [ onClick (ShowEditCreateRecipe (Just ( id, recipe ))) ] [ text "edit" ]
                ]

        EditRecipeView ( maybeId, recipe ) ->
            div []
                [ h3 [] [ text "Add a new Recipe" ]
                , p []
                    [ label [] [ text "Name:" ]
                    , viewInput "text" recipe.name (\a -> EditRecipe maybeId recipe (ChangedName a))
                    ]
                , p []
                    [ label [] [ text "Ingredients:" ]
                    , viewInput "text" recipe.ingredients (\a -> EditRecipe maybeId recipe (ChangedIngredients a))
                    ]
                , p []
                    [ label [] [ text "Instructions:" ]
                    , viewInput "text" recipe.instructions (\a -> EditRecipe maybeId recipe (ChangedInstructions a))
                    ]
                , button [ onClick ShowList ] [ text "discard" ]
                , button [ onClick (AddRecipe maybeId recipe) ] [ text "save" ]
                ]


createRecipesListView : List Recipe -> Html Msg
createRecipesListView recipes =
    div []
        [ h1 [] [ text "List of Recipes" ]
        , ul [] (List.map createRecipeView (List.indexedMap Tuple.pair recipes))
        ]


createRecipeView : ( Int, Recipe ) -> Html Msg
createRecipeView ( id, recipe ) =
    li [ onClick (ShowSingle id recipe) ] [ text recipe.name ]


viewInput : String -> String -> (String -> msg) -> Html msg
viewInput t v toMsg =
    input [ type_ t, value v, onInput toMsg ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
