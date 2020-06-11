module Ingredient exposing (..)

import Json.Decode exposing (Decoder, field, int, list, map3, string)


type alias Ingredient =
    { id : Int, name : String, amount : Int }


ingredientDecoder : Decoder Ingredient
ingredientDecoder =
    map3 Ingredient
        (field "foodStuffId" int)
        (field "name" string)
        (field "amount" int)


ingredientListDecoder : Decoder (List Ingredient)
ingredientListDecoder =
    list ingredientDecoder
