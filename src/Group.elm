-- Copyright (c) 2026 Derek Gustafson.


module Group exposing (Game, Group, GroupEdit, applyEdit, displayName, feedName, findGroup, gameName, newGameButtons, newGroup, themeClass, updateGroup, viewGroupBody)

import Html exposing (Html, button, div, h3, input, label, li, ol, option, p, select, span, text, textarea, ul)
import Html.Attributes exposing (class, disabled, selected, title, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (removeAt, updateAt)


type Game
    = MorkBorg
    | CyBorg


themeClass : Maybe Game -> String
themeClass g =
    case g of
        Nothing ->
            "theme-base"

        Just game ->
            case game of
                MorkBorg ->
                    "theme-mb"

                CyBorg ->
                    "theme-cb"


newGameButtons : (Game -> msg) -> List (Html msg)
newGameButtons f =
    [ button
        [ onClick (f MorkBorg) ]
        [ text "New Mork Borg group" ]
    , button
        [ class "btn-secondary"
        , onClick (f CyBorg)
        ]
        [ text "New Cy_Borg group" ]
    ]


gameName : Game -> String
gameName game =
    case game of
        MorkBorg ->
            "Mork Borg"

        CyBorg ->
            "Cy_Borg"


counterName : Game -> String
counterName game =
    case game of
        MorkBorg ->
            "Miseries"

        CyBorg ->
            "Miserable Headlines"


currencyName : Game -> String
currencyName game =
    case game of
        MorkBorg ->
            "Silver"

        CyBorg ->
            "Credits"


omenName : Game -> String
omenName game =
    case game of
        MorkBorg ->
            "Omens"

        CyBorg ->
            "Glitches"


feedName : Maybe Game -> String
feedName mg =
    case mg of
        Nothing ->
            "Doomed feed"

        Just game ->
            case game of
                MorkBorg ->
                    "Wretched Feed"

                CyBorg ->
                    "Netrunner Wall"


defaultStats : Game -> CharacterStats
defaultStats game =
    case game of
        MorkBorg ->
            MbCharacterStats
                { agility = 0
                , presence = 0
                , strength = 0
                , toughness = 0
                }

        CyBorg ->
            CbCharacterStats
                { agility = 0
                , knowledge = 0
                , presence = 0
                , strength = 0
                , toughness = 0
                }


type alias MbStats =
    { agility : Int
    , presence : Int
    , strength : Int
    , toughness : Int
    }


type alias CbStats =
    { agility : Int
    , knowledge : Int
    , presence : Int
    , strength : Int
    , toughness : Int
    }


type CharacterStats
    = MbCharacterStats MbStats
    | CbCharacterStats CbStats


type MbStatField
    = MbAgility
    | MbPresence
    | MbStrength
    | MbToughness


type CbStatField
    = CbAgility
    | CbKnowledge
    | CbPresence
    | CbStrength
    | CbToughness


type alias Character =
    { name : String
    , class : String
    , omens : Int
    , stats : CharacterStats
    , isOpen : Bool
    }


type alias Group =
    { id : Int
    , game : Game
    , name : String
    , currency : Int
    , characters : List Character
    , inventory : List String
    , notes : String
    , misery : Int
    , newItem : String
    , newCharacter : String
    }


type GroupEdit
    = SetName String
    | SetCurrency String
    | SetNotes String
    | RemoveItem Int
    | AdjustMisery Int
    | ToggleCharacter Int
    | RemoveCharacter Int
    | SetMbStat Int MbStatField Int
    | SetCbStat Int CbStatField Int
    | SetNewItem String
    | SubmitItem
    | SetNewCharacter String
    | SubmitCharacter
    | SetClass Int String
    | SetOmens Int Int


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
    , currency = 0
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


adjustMbStatRecord : MbStatField -> Int -> MbStats -> MbStats
adjustMbStatRecord field delta s =
    case field of
        MbAgility ->
            { s | agility = s.agility + delta }

        MbPresence ->
            { s | presence = s.presence + delta }

        MbStrength ->
            { s | strength = s.strength + delta }

        MbToughness ->
            { s | toughness = s.toughness + delta }


adjustCbStatRecord : CbStatField -> Int -> CbStats -> CbStats
adjustCbStatRecord field delta s =
    case field of
        CbAgility ->
            { s | agility = s.agility + delta }

        CbKnowledge ->
            { s | knowledge = s.knowledge + delta }

        CbPresence ->
            { s | presence = s.presence + delta }

        CbStrength ->
            { s | strength = s.strength + delta }

        CbToughness ->
            { s | toughness = s.toughness + delta }


setMbStatRecord : MbStatField -> Int -> MbStats -> MbStats
setMbStatRecord field val s =
    let
        clamped =
            clamp -3 3 val
    in
    case field of
        MbAgility ->
            { s | agility = clamped }

        MbPresence ->
            { s | presence = clamped }

        MbStrength ->
            { s | strength = clamped }

        MbToughness ->
            { s | toughness = clamped }


setCbStatRecord : CbStatField -> Int -> CbStats -> CbStats
setCbStatRecord field val s =
    let
        clamped =
            clamp -3 3 val
    in
    case field of
        CbAgility ->
            { s | agility = clamped }

        CbKnowledge ->
            { s | knowledge = clamped }

        CbPresence ->
            { s | presence = clamped }

        CbStrength ->
            { s | strength = clamped }

        CbToughness ->
            { s | toughness = clamped }


maxMisery : Int
maxMisery =
    7


applyEdit : GroupEdit -> Group -> Group
applyEdit edit group =
    case edit of
        SetName name ->
            { group | name = name }

        SetCurrency s ->
            { group | currency = max 0 (Maybe.withDefault 0 (String.toInt s)) }

        SetNotes notes ->
            { group | notes = notes }

        RemoveItem index ->
            { group | inventory = removeAt index group.inventory }

        AdjustMisery delta ->
            { group | misery = clamp 0 maxMisery (group.misery + delta) }

        ToggleCharacter index ->
            { group
                | characters =
                    updateAt index (\c -> { c | isOpen = not c.isOpen }) group.characters
            }

        RemoveCharacter index ->
            { group | characters = removeAt index group.characters }

        SetMbStat charIndex field val ->
            { group
                | characters =
                    updateAt charIndex
                        (\c ->
                            case c.stats of
                                MbCharacterStats mb ->
                                    { c | stats = MbCharacterStats (setMbStatRecord field val mb) }

                                other ->
                                    { c | stats = other }
                        )
                        group.characters
            }

        SetCbStat charIndex field val ->
            { group
                | characters =
                    updateAt charIndex
                        (\c ->
                            case c.stats of
                                CbCharacterStats cb ->
                                    { c | stats = CbCharacterStats (setCbStatRecord field val cb) }

                                other ->
                                    { c | stats = other }
                        )
                        group.characters
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
                    String.trim <| group.newCharacter
            in
            if String.isEmpty name then
                group

            else
                { group
                    | characters =
                        group.characters
                            ++ [ { name = name, class = "Classless", omens = 0, stats = defaultStats group.game, isOpen = False } ]
                    , newCharacter = ""
                }

        SetClass charIndex class ->
            { group
                | characters =
                    updateAt charIndex
                        (\c -> { c | class = class })
                        group.characters
            }

        SetOmens charIndex omens ->
            { group
                | characters =
                    updateAt charIndex
                        (\c -> { c | omens = omens })
                        group.characters
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


viewCharacters : (GroupEdit -> msg) -> Game -> String -> List Character -> Html msg
viewCharacters f game newCharacter characters =
    div []
        [ h3 [] [ text "Characters" ]
        , if List.isEmpty characters then
            p [] [ text "No characters yet." ]

          else
            ol [] <| List.indexedMap (viewCharacter f game) characters
        , div []
            [ input [ type_ "text", value newCharacter, onInput (f << SetNewCharacter) ] []
            , button [ onClick (f SubmitCharacter) ] [ text "Add character" ]
            ]
        ]


viewInventory : (GroupEdit -> msg) -> String -> List String -> Html msg
viewInventory f newItem items =
    div []
        [ h3 [] [ text "Inventory" ]
        , if List.isEmpty items then
            p [] [ text "Nothing yet." ]

          else
            ul [] (List.indexedMap (viewItem f) items)
        , div []
            [ input [ type_ "text", value newItem, onInput (f << SetNewItem) ] []
            , button [ onClick <| f SubmitItem ] [ text "Add item" ]
            ]
        ]


statOptions : Int -> Int -> Int -> List (Html msg)
statOptions lb ub currentVal =
    List.range lb ub
        |> List.map
            (\n ->
                option
                    [ value (String.fromInt n)
                    , selected (n == currentVal)
                    ]
                    [ text (String.fromInt n) ]
            )


viewStatSelect : String -> Int -> (Int -> msg) -> Html msg
viewStatSelect statLabel currentVal toMsg =
    label [ class "stat-row", title statLabel ]
        [ span [ class "stat-label" ] [ text (String.toUpper (String.left 3 statLabel)) ]
        , select
            [ class "stat-select"
            , onInput
                (\s ->
                    String.toInt s
                        |> Maybe.withDefault 0
                        |> toMsg
                )
            ]
            (statOptions -3 6 currentVal)
        ]


viewStats : (GroupEdit -> msg) -> Int -> CharacterStats -> Html msg
viewStats f charIndex stats =
    div [ class "character-stats" ]
        (case stats of
            MbCharacterStats mb ->
                [ viewStatSelect "Agility" mb.agility (f << SetMbStat charIndex MbAgility)
                , viewStatSelect "Presence" mb.presence (f << SetMbStat charIndex MbPresence)
                , viewStatSelect "Strength" mb.strength (f << SetMbStat charIndex MbStrength)
                , viewStatSelect "Toughness" mb.toughness (f << SetMbStat charIndex MbToughness)
                ]

            CbCharacterStats cb ->
                [ viewStatSelect "Agility" cb.agility (f << SetCbStat charIndex CbAgility)
                , viewStatSelect "Knowledge" cb.knowledge (f << SetCbStat charIndex CbKnowledge)
                , viewStatSelect "Presence" cb.presence (f << SetCbStat charIndex CbPresence)
                , viewStatSelect "Strength" cb.strength (f << SetCbStat charIndex CbStrength)
                , viewStatSelect "Toughness" cb.toughness (f << SetCbStat charIndex CbToughness)
                ]
        )


viewOmenSelect : String -> Int -> (Int -> msg) -> Html msg
viewOmenSelect statLabel currentVal toMsg =
    label [ class "stat-row", title statLabel ]
        [ span [ class "stat-label" ] [ text statLabel ]
        , select
            [ class "stat-select"
            , onInput
                (\s ->
                    String.toInt s
                        |> Maybe.withDefault 0
                        |> toMsg
                )
            ]
            (statOptions 0 10 currentVal)
        ]


viewCharacter : (GroupEdit -> msg) -> Game -> Int -> Character -> Html msg
viewCharacter f game index character =
    li []
        (button
            [ class "btn-secondary"
            , onClick <| f <| ToggleCharacter index
            ]
            [ text character.name ]
            :: (if character.isOpen then
                    [ div [ class "character-stats" ]
                        [ input [ type_ "text", value character.class, onInput (f << SetClass index) ] []
                        , viewOmenSelect (omenName game) character.omens (f << SetOmens index)
                        ]
                    , viewStats f index character.stats
                    , button [ onClick <| f <| RemoveCharacter index ] [ text "Remove" ]
                    ]

                else
                    []
               )
        )


viewItem : (GroupEdit -> msg) -> Int -> String -> Html msg
viewItem f index item =
    li []
        [ text (item ++ " ")
        , button [ onClick <| f <| RemoveItem index ] [ text "Remove" ]
        ]


viewGroupBody : (GroupEdit -> msg) -> Group -> List (Html msg)
viewGroupBody f group =
    [ div []
        [ label []
            [ text "Party name "
            , input [ type_ "text", value group.name, onInput (f << SetName) ] []
            ]
        ]
    , div []
        [ label []
            [ text <| currencyName group.game
            , input
                [ type_ "number"
                , value (String.fromInt group.currency)
                , onInput (f << SetCurrency)
                ]
                []
            ]
        ]
    , viewMisery f group
    , viewCharacters f group.game group.newCharacter group.characters
    , viewInventory f group.newItem group.inventory
    , div []
        [ h3 [] [ text "Notes" ]
        , textarea [ value group.notes, onInput (f << SetNotes) ] []
        ]
    ]
