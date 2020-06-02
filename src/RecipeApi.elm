module RecipeApi exposing (..)

import Http
import Recipe exposing (NewRecipe, Recipe, RecipeOverview, recipeDecoder, recipeListDecoder)


baseUrl =
    "http://localhost:8080/recipe"


type RecipeApiResultMsg
    = LoadedAllRecipes (Result Http.Error (List RecipeOverview))
    | LoadedSingleRecipe (Result Http.Error Recipe)
    | SubmittedNewRecipe (Result Http.Error Recipe)



--  | SubmittedRecipe (Result Http.Error Recipe)


getAllRecipes : Cmd RecipeApiResultMsg
getAllRecipes =
    Http.get
        { url = baseUrl
        , expect = Http.expectJson LoadedAllRecipes recipeListDecoder
        }


getSingeRecipe : Int -> Cmd RecipeApiResultMsg
getSingeRecipe id =
    Http.get
        { url = baseUrl ++ "/" ++ String.fromInt id
        , expect = Http.expectJson LoadedSingleRecipe recipeDecoder
        }


submitNewRecipe : NewRecipe -> Cmd RecipeApiResultMsg
submitNewRecipe newRecipe =
    Http.post
        { url = baseUrl
        , body = Http.jsonBody (Recipe.newRecipeEncoder newRecipe)
        , expect = Http.expectJson SubmittedNewRecipe recipeDecoder
        }


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
