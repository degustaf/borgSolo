-- Copyright (c) 2026 Derek Gustafson.


module Group exposing (Group, GroupEdit, applyEdit, displayName, encodeGroup, findGroup, groupDecoder, newGroup, updateGroup, viewGroupBody)

import Character exposing (Character, CharacterEdit, Game, characterDecoder, counterName, encodeCharacter, encodeGame, gameDecoder, newCharacter, viewCharacter)
import Html exposing (Html, button, div, h3, input, label, li, ol, text, textarea, ul)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (removeAt, updateAt)


type alias Group =
    { id : Int
    , game : Game
    , name : String
    , characters : List Character
    , inventory : List String
    , notes : String
    , misery : Int
    , newItem : String
    , newCharacter : String
    }


encodeGroup : Group -> Encode.Value
encodeGroup g =
    Encode.object
        [ ( "id", Encode.int g.id )
        , ( "game", encodeGame g.game )
        , ( "name", Encode.string g.name )
        , ( "characters", Encode.list encodeCharacter g.characters )
        , ( "inventory", Encode.list Encode.string g.inventory )
        , ( "notes", Encode.string g.notes )
        , ( "misery", Encode.int g.misery )
        , ( "newItem", Encode.string g.newItem )
        , ( "newCharacter", Encode.string g.newCharacter )
        ]


groupDecoder : Decode.Decoder Group
groupDecoder =
    let
        andMap argFunc func =
            Decode.map2 (<|) func argFunc
    in
    Decode.succeed Group
        |> andMap (Decode.field "id" Decode.int)
        |> andMap (Decode.field "game" gameDecoder)
        |> andMap (Decode.field "name" Decode.string)
        |> andMap (Decode.field "characters" (Decode.list characterDecoder))
        |> andMap (Decode.field "inventory" (Decode.list Decode.string))
        |> andMap (Decode.field "notes" Decode.string)
        |> andMap (Decode.field "misery" Decode.int)
        |> andMap (Decode.field "newItem" Decode.string)
        |> andMap (Decode.field "newCharacter" Decode.string)


type GroupEdit
    = SetName String
    | SetNotes String
    | RemoveItem Int
    | AdjustMisery Int
    | RemoveCharacter Int
    | EditCharacter Int CharacterEdit
    | SetNewItem String
    | SubmitItem
    | SetNewCharacter String
    | SubmitCharacter


updateGroup : Int -> (Group -> Group) -> List Group -> List Group
updateGroup id f =
    List.map
        (\g ->
            if g.id == id then
                f g

            else
                g
        )


newGroup : Int -> Game -> Group
newGroup id game =
    { id = id
    , game = game
    , name = "Party " ++ String.fromInt id
    , characters = []
    , inventory = []
    , notes = ""
    , misery = 0
    , newItem = ""
    , newCharacter = ""
    }


displayName : Group -> String
displayName group =
    if String.isEmpty (String.trim group.name) then
        "Unnamed party"

    else
        group.name


findGroup : Int -> List Group -> Maybe Group
findGroup id groups =
    List.filter (\g -> g.id == id) groups
        |> List.head


maxMisery : Int
maxMisery =
    7


applyEdit : GroupEdit -> Group -> Group
applyEdit edit group =
    case edit of
        SetName name ->
            { group | name = name }

        SetNotes notes ->
            { group | notes = notes }

        RemoveItem index ->
            { group | inventory = removeAt index group.inventory }

        AdjustMisery delta ->
            { group | misery = clamp 0 maxMisery (group.misery + delta) }

        RemoveCharacter index ->
            { group | characters = removeAt index group.characters }

        EditCharacter charIndex charEdit ->
            { group
                | characters =
                    updateAt charIndex (Character.applyEdit charEdit) group.characters
            }

        SetNewItem item ->
            { group | newItem = item }

        SubmitItem ->
            let
                item =
                    String.trim group.newItem
            in
            if String.isEmpty item then
                group

            else
                { group | inventory = group.inventory ++ [ item ], newItem = "" }

        SetNewCharacter char ->
            { group | newCharacter = char }

        SubmitCharacter ->
            let
                name =
                    String.trim group.newCharacter
            in
            if String.isEmpty name then
                group

            else
                { group
                    | characters = group.characters ++ [ newCharacter group.game name ]
                    , newCharacter = ""
                }


viewMisery : (GroupEdit -> msg) -> Group -> Html msg
viewMisery f group =
    div []
        [ text (counterName group.game ++ ": ")
        , text (" " ++ String.fromInt group.misery ++ " / " ++ String.fromInt maxMisery ++ " ")
        , button
            [ onClick <| f <| AdjustMisery 1
            , disabled (group.misery >= maxMisery)
            ]
            [ text "+" ]
        ]


viewCharacterRow : (GroupEdit -> msg) -> Int -> Character -> Html msg
viewCharacterRow f index character =
    viewCharacter
        { onRemove = f (RemoveCharacter index)
        , onEdit = \charEdit -> f (EditCharacter index charEdit)
        , character = character
        }


viewCharacters : (GroupEdit -> msg) -> String -> List Character -> Html msg
viewCharacters f newCharacter characters =
    div []
        [ h3 [] [ text "Characters" ]
        , ol [] <| List.indexedMap (viewCharacterRow f) characters
        , div []
            [ input [ type_ "text", value newCharacter, onInput (f << SetNewCharacter) ] []
            , button [ onClick (f SubmitCharacter) ] [ text "Add character" ]
            ]
        ]


viewItem : (Int -> msg) -> Int -> String -> Html msg
viewItem f index item =
    li []
        [ text (item ++ " ")
        , button [ onClick <| f index ] [ text "Remove" ]
        ]


viewInventory : (GroupEdit -> msg) -> String -> List String -> Html msg
viewInventory f newItem items =
    div []
        [ h3 [] [ text "Party Inventory" ]
        , ul [] (List.indexedMap (viewItem (f << RemoveItem)) items)
        , div []
            [ input [ type_ "text", value newItem, onInput (f << SetNewItem) ] []
            , button [ onClick <| f SubmitItem ] [ text "Add item" ]
            ]
        ]


viewGroupBody : (GroupEdit -> msg) -> Group -> List (Html msg)
viewGroupBody f group =
    [ div []
        [ label []
            [ text "Party name "
            , input [ type_ "text", value group.name, onInput (f << SetName) ] []
            ]
        ]
    , viewMisery f group
    , viewCharacters f group.newCharacter group.characters
    , viewInventory f group.newItem group.inventory
    , div []
        [ h3 [] [ text "Party Notes" ]
        , textarea [ value group.notes, onInput (f << SetNotes) ] []
        ]
    ]
