-- Copyright (c) 2026 Derek Gustafson.


module Group exposing (Group, GroupEdit, applyEdit, displayName, findGroup, newGroup, updateGroup, viewGroupBody)

import Character exposing (Character, CharacterEdit, Game, counterName, currencyName, newCharacter, omenName, toggleOpen, viewCharacter)
import Html exposing (Html, button, div, h3, input, label, li, ol, text, textarea, ul)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onClick, onInput)
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


type GroupEdit
    = SetName String
    | SetNotes String
    | RemoveItem Int
    | AdjustMisery Int
    | ToggleCharacter Int
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

        ToggleCharacter index ->
            { group
                | characters =
                    updateAt index toggleOpen group.characters
            }

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


viewCharacterRow : (GroupEdit -> msg) -> Game -> Int -> Character -> Html msg
viewCharacterRow f game index character =
    viewCharacter
        { onToggle = f (ToggleCharacter index)
        , onRemove = f (RemoveCharacter index)
        , onEdit = \charEdit -> f (EditCharacter index charEdit)
        , currencyLabel = currencyName game
        , omenLabel = omenName game
        , character = character
        }


viewCharacters : (GroupEdit -> msg) -> Game -> String -> List Character -> Html msg
viewCharacters f game newCharacter characters =
    div []
        [ h3 [] [ text "Characters" ]
        , ol [] <| List.indexedMap (viewCharacterRow f game) characters
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
    , viewCharacters f group.game group.newCharacter group.characters
    , viewInventory f group.newItem group.inventory
    , div []
        [ h3 [] [ text "Pary Notes" ]
        , textarea [ value group.notes, onInput (f << SetNotes) ] []
        ]
    ]
