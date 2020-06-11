module Cookbook exposing (..)

import ApiHelper exposing (httpErrorString)
import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Bulma.Elements exposing (TableRow, TitleSize(..), button, content, table, tableBody, tableCell, tableCellHead, tableFoot, tableHead, tableModifiers, tableRow, title)
import Bulma.Form exposing (control, controlHelp, controlInputModifiers, controlLabel, controlText, controlTextArea, controlTextAreaModifiers, field, fields)
import Bulma.Layout exposing (SectionSpacing(..), container, hero, heroBody, heroModifiers, section)
import Bulma.Modifiers exposing (Color(..), Size(..))
import FoodStuff exposing (FoodStuffOverview, foodStuffListDecoder)
import Html exposing (Html, li, p, text, ul)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)
import Http
import Ingredient exposing (Ingredient)
import Recipe exposing (NewRecipe, Recipe, RecipeOverview)
import RecipeApi exposing (RecipeApiResultMsg(..), getAllRecipes, getSingeRecipe)
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- Model


type Session
    = Guest Nav.Key
    | LoggedIn Nav.Key String


type ModelState
    = Loading
    | Failure String
    | ViewAllRecipes (List RecipeOverview)
    | ViewSingleRecipe Recipe
    | CreateNewRecipe NewRecipe
    | FoodStuffPage FoodStuff.Model


type alias Model =
    { state : ModelState
    , key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model (FoodStuffPage FoodStuff.Loading) key url
    , Http.get
        { url = "http://localhost:8080/foodstuff"
        , expect = Http.expectJson (\a -> FoodStuffMsg (FoodStuff.CompletedLoadFoodstuffList a)) foodStuffListDecoder
        }
    )



-- Update


type CreateAction
    = UpdateName String
    | UpdateDescription String
    | UpdateInstructions String
    | UpdateNewIngredient String
    | AddIngredient String
    | RemoveIngredient String
    | SubmitNewRecipe


type Msg
    = RecipeSelected Int
    | AllRecipesSelected
    | CreateNewRecipeSelected
    | ApiResult RecipeApiResultMsg
    | FoodStuffMsg FoodStuff.Msg
    | CreateRecipeMsg CreateAction
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


updateModelStateWith : (subModel -> ModelState) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateModelStateWith toModel toMsg model ( subModel, subCmd ) =
    ( { model | state = toModel subModel }
    , Cmd.map toMsg subCmd
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RecipeSelected int ->
            ( model
            , Cmd.map ApiResult (getSingeRecipe int)
            )

        AllRecipesSelected ->
            ( model
            , Cmd.map ApiResult getAllRecipes
            )

        ApiResult r ->
            { model | state = handleApiResult r } |> noCmd

        CreateNewRecipeSelected ->
            { model | state = CreateNewRecipe Recipe.emptyNewRecipe } |> noCmd

        CreateRecipeMsg createAction ->
            case model.state of
                CreateNewRecipe newRecipe ->
                    let
                        newm =
                            handleCreateMsg newRecipe createAction
                    in
                    ( { model | state = Tuple.first newm }, Tuple.second newm )

                _ ->
                    { model | state = Failure "received createAction without matching model state" } |> noCmd

        LinkClicked urlRequest ->
            -- TODO
            model |> noCmd

        UrlChanged url ->
            -- TODO
            model |> noCmd

        FoodStuffMsg m ->
            case model.state of
                FoodStuffPage fm ->
                    FoodStuff.update m fm |> updateModelStateWith FoodStuffPage FoodStuffMsg model

                _ ->
                    { model | state = Failure "received foodstuff msg without matching model state" } |> noCmd


handleApiResult : RecipeApiResultMsg -> ModelState
handleApiResult msg =
    case msg of
        LoadedAllRecipes (Ok value) ->
            ViewAllRecipes value

        LoadedAllRecipes (Err error) ->
            Failure (httpErrorString error)

        LoadedSingleRecipe (Ok recipe) ->
            ViewSingleRecipe recipe

        LoadedSingleRecipe (Err error) ->
            Failure (httpErrorString error)

        SubmittedNewRecipe (Ok recipe) ->
            ViewSingleRecipe recipe

        SubmittedNewRecipe (Err error) ->
            Failure (httpErrorString error)


handleCreateMsg : NewRecipe -> CreateAction -> ( ModelState, Cmd Msg )
handleCreateMsg newRecipe createAction =
    case createAction of
        UpdateName string ->
            CreateNewRecipe { newRecipe | name = string } |> noCmd

        UpdateDescription string ->
            CreateNewRecipe { newRecipe | description = string } |> noCmd

        UpdateInstructions string ->
            CreateNewRecipe { newRecipe | instructions = string } |> noCmd

        AddIngredient string ->
            CreateNewRecipe { newRecipe | ingredients = string :: newRecipe.ingredients, newIngredient = "" } |> noCmd

        RemoveIngredient string ->
            CreateNewRecipe { newRecipe | ingredients = List.filter (\a -> a /= string) newRecipe.ingredients } |> noCmd

        UpdateNewIngredient string ->
            CreateNewRecipe { newRecipe | newIngredient = string } |> noCmd

        SubmitNewRecipe ->
            ( CreateNewRecipe newRecipe, Cmd.map ApiResult (RecipeApi.submitNewRecipe newRecipe) )


noCmd : m -> ( m, Cmd Msg )
noCmd model =
    ( model, Cmd.none )



-- VIEW


createRecipeRow : RecipeOverview -> TableRow Msg
createRecipeRow recipe =
    tableRow False
        [ onClick (RecipeSelected recipe.id) ]
        [ tableCell [] [ text recipe.name ]
        , tableCell [] [ text recipe.description ]
        ]


recipeTable : List RecipeOverview -> Bulma.Elements.Table Msg
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


recipeHeader : Bulma.Layout.Hero msg
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


viewInPage : List (Html msg) -> Html msg
viewInPage content =
    container []
        [ recipeHeader
        , section NotSpaced
            []
            content
        ]


listElement : Ingredient -> Html msg
listElement i =
    Html.li [] [ text (String.fromInt i.amount ++ " " ++ i.name) ]


viewRecipeContent : Recipe -> Html Msg
viewRecipeContent recipe =
    content Standard
        []
        [ title H1 [] [ text recipe.name ]
        , p [] [ text recipe.description ]
        , title H4 [] [ text "Ingredients" ]
        , ul []
            (List.map listElement recipe.ingredients)
        , title H4 [] [ text "Instructions" ]
        , p [] [ text recipe.instructions ]
        ]


createButton m t =
    Bulma.Elements.button Bulma.Elements.buttonModifiers [ onClick m ] [ text t ]


backToAllRecipesButton =
    createButton AllRecipesSelected "Back"


sectionedContentList : List (Html msg) -> List (Html msg)
sectionedContentList contentList =
    List.map (\a -> section NotSpaced [] [ a ]) contentList


viewRecipe recipe =
    sectionedContentList [ viewRecipeContent recipe, backToAllRecipesButton ]


displayIngredientList : List String -> Html Msg
displayIngredientList ingredients =
    ul [] (List.map displayIngredient ingredients)


displayIngredient : String -> Html Msg
displayIngredient ingredient =
    li [ onClick (CreateRecipeMsg (RemoveIngredient ingredient)) ] [ text ingredient ]



-- add remove button


createIngredientThingy : List String -> String -> Html Msg
createIngredientThingy ingredients newIngredient =
    field []
        [ controlLabel [] [ text "Ingredients: " ]
        , Bulma.Form.connectedFields Bulma.Modifiers.Centered
            []
            [ controlText { controlInputModifiers | expanded = True } [] [ onInput (\a -> CreateRecipeMsg (UpdateNewIngredient a)), placeholder "New ingredient", Html.Attributes.value newIngredient ] []
            , control Bulma.Form.controlModifiers
                []
                [ button Bulma.Elements.buttonModifiers [ onClick (CreateRecipeMsg (AddIngredient newIngredient)) ] [ text "+" ]
                ]
            ]
        , displayIngredientList ingredients
        ]


createRecipeForm : NewRecipe -> Html Msg
createRecipeForm recipe =
    container []
        [ field []
            [ controlLabel [] [ text ("Recipe Name: " ++ recipe.name) ]
            , controlText controlInputModifiers [] [ onInput (\a -> CreateRecipeMsg (UpdateName a)), placeholder "Name", Html.Attributes.value recipe.name ] []
            , controlHelp Default [] []
            ]
        , field []
            [ controlLabel [] [ text ("Recipe Description: " ++ recipe.description) ]
            , controlText controlInputModifiers [] [ onInput (\a -> CreateRecipeMsg (UpdateDescription a)), placeholder "Description", Html.Attributes.value recipe.description ] []
            , controlHelp Default [] []
            ]
        , createIngredientThingy recipe.ingredients recipe.newIngredient
        , field []
            [ controlLabel [] [ text ("Recipe Instructions: " ++ recipe.instructions) ]
            , controlTextArea controlTextAreaModifiers [] [ onInput (\a -> CreateRecipeMsg (UpdateInstructions a)), placeholder "Instructions", Html.Attributes.value recipe.instructions ] []
            , controlHelp Default [] []
            ]
        ]


viewCreateRecipePage : NewRecipe -> List (Html Msg)
viewCreateRecipePage recipe =
    sectionedContentList
        [ title H1 [] [ text "Create new recipe" ]
        , createRecipeForm recipe
        , p [] [ text (Recipe.testRecipeEncoder recipe) ]
        , fields Bulma.Modifiers.Left [] [ backToAllRecipesButton, createButton (CreateRecipeMsg SubmitNewRecipe) "Create" ]
        ]


viewBody : ModelState -> Html Msg
viewBody model =
    case model of
        Loading ->
            viewInPage [ text "loading.." ]

        ViewAllRecipes recipes ->
            viewInPage
                (sectionedContentList
                    [ title H1 [] [ text "All recipes" ]
                    , recipeTable recipes
                    , createButton CreateNewRecipeSelected "create new"
                    ]
                )

        Failure e ->
            viewInPage [ text "failed: ", text e ]

        ViewSingleRecipe recipe ->
            viewInPage (viewRecipe recipe)

        CreateNewRecipe recipe ->
            viewInPage (viewCreateRecipePage recipe)

        FoodStuffPage m ->
            Html.map FoodStuffMsg (viewInPage [ FoodStuff.view m ])


view : Model -> Browser.Document Msg
view model =
    { title = "The cookbook title", body = [ viewBody model.state ] }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
