module Recipe exposing (..)

import FoodStuff exposing (Ingredient)
import Json.Decode exposing (Decoder, field, int, list, map3, map5, string)
import Json.Encode as Encode


type alias RecipeOverview =
    { id : Int, name : String, description : String }


type alias Recipe =
    { id : Int, name : String, description : String, ingredients : List Ingredient, instructions : String }


type alias NewRecipe =
    { name : String, description : String, ingredients : List String, instructions : String, newIngredient : String }


emptyNewRecipe : NewRecipe
emptyNewRecipe =
    { name = "", description = "", ingredients = [], instructions = "", newIngredient = "" }


newRecipeEncoder : NewRecipe -> Encode.Value
newRecipeEncoder newRecipe =
    Encode.object
        [ ( "name", Encode.string newRecipe.name )
        , ( "description", Encode.string newRecipe.description )
        , ( "ingredients", Encode.list Encode.string newRecipe.ingredients )
        , ( "instructions", Encode.string newRecipe.instructions )
        ]


testRecipeEncoder : NewRecipe -> String
testRecipeEncoder newRecipe =
    Encode.encode 4 (newRecipeEncoder newRecipe)


recipeDecoder : Decoder Recipe
recipeDecoder =
    map5 Recipe
        (field "id" int)
        (field "name" string)
        (field "description" string)
        (field "ingredients" FoodStuff.ingredientListDecoder)
        (field "instructions" string)


recipeOverviewDecoder : Decoder RecipeOverview
recipeOverviewDecoder =
    map3 RecipeOverview
        (field "id" int)
        (field "name" string)
        (field "description" string)


recipeListDecoder : Decoder (List RecipeOverview)
recipeListDecoder =
    list recipeOverviewDecoder
