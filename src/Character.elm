-- Copyright (c) 2026 Derek Gustafson.


module Character exposing (Character, CharacterEdit, Game, applyEdit, counterName, currencyName, feedName, gameName, newCharacter, newGameButtons, omenName, themeClass, toggleOpen, viewCharacter)

import Html exposing (Html, button, div, h3, input, label, li, option, select, span, text, textarea, ul)
import Html.Attributes exposing (class, selected, title, type_, value)
import Html.Events exposing (onClick, onInput)
import List.Extra exposing (removeAt)


type Game
    = MorkBorg
    | CyBorg


isMorkBorg : Game -> Bool
isMorkBorg game =
    case game of
        MorkBorg ->
            True

        CyBorg ->
            False


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
    , hp : Int
    , maxHp : Int
    , currency : Int
    , omens : Int
    , stats : CharacterStats
    , inventory : List String
    , newItem : String
    , notes : String
    , isOpen : Bool
    }


type CharacterEdit
    = SetClass String
    | SetHp Int
    | SetMaxHp String
    | SetCurrency String
    | SetOmens Int
    | SetMbStat MbStatField Int
    | SetCbStat CbStatField Int
    | SetNewItem String
    | SubmitItem
    | RemoveItem Int
    | SetNotes String


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


newCharacter : Game -> String -> Character
newCharacter game name =
    { name = name
    , class = "Classless"
    , hp = 1
    , maxHp = 1
    , currency = 0
    , omens = 0
    , stats = defaultStats game
    , inventory = []
    , newItem = ""
    , notes = ""
    , isOpen = False
    }


toggleOpen : Character -> Character
toggleOpen c =
    { c | isOpen = not c.isOpen }


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


applyEdit : CharacterEdit -> Character -> Character
applyEdit edit c =
    case edit of
        SetClass class ->
            { c | class = class }

        SetHp hp ->
            { c | hp = clamp 0 c.maxHp hp }

        SetMaxHp maxHpStr ->
            let
                newMax =
                    max 0 (Maybe.withDefault 0 (String.toInt maxHpStr))
            in
            { c
                | maxHp = newMax
                , hp = clamp 0 newMax c.hp
            }

        SetCurrency s ->
            { c | currency = max 0 (Maybe.withDefault 0 (String.toInt s)) }

        SetOmens omens ->
            { c | omens = omens }

        SetMbStat field val ->
            case c.stats of
                MbCharacterStats mb ->
                    { c | stats = MbCharacterStats (setMbStatRecord field val mb) }

                other ->
                    { c | stats = other }

        SetCbStat field val ->
            case c.stats of
                CbCharacterStats cb ->
                    { c | stats = CbCharacterStats (setCbStatRecord field val cb) }

                other ->
                    { c | stats = other }

        SetNewItem item ->
            { c | newItem = item }

        SubmitItem ->
            let
                item =
                    String.trim c.newItem
            in
            if String.isEmpty item then
                c

            else
                { c
                    | inventory = c.inventory ++ [ item ]
                    , newItem = ""
                }

        RemoveItem itemIndex ->
            { c | inventory = removeAt itemIndex c.inventory }

        SetNotes notes ->
            { c | notes = notes }


intOptions : Int -> Int -> Int -> List (Html msg)
intOptions lb ub currentVal =
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
            (intOptions -3 6 currentVal)
        ]


viewStats : (CharacterEdit -> msg) -> CharacterStats -> Html msg
viewStats f stats =
    div [ class "character-stats" ]
        (case stats of
            MbCharacterStats mb ->
                [ viewStatSelect "Agility" mb.agility (f << SetMbStat MbAgility)
                , viewStatSelect "Presence" mb.presence (f << SetMbStat MbPresence)
                , viewStatSelect "Strength" mb.strength (f << SetMbStat MbStrength)
                , viewStatSelect "Toughness" mb.toughness (f << SetMbStat MbToughness)
                ]

            CbCharacterStats cb ->
                [ viewStatSelect "Agility" cb.agility (f << SetCbStat CbAgility)
                , viewStatSelect "Knowledge" cb.knowledge (f << SetCbStat CbKnowledge)
                , viewStatSelect "Presence" cb.presence (f << SetCbStat CbPresence)
                , viewStatSelect "Strength" cb.strength (f << SetCbStat CbStrength)
                , viewStatSelect "Toughness" cb.toughness (f << SetCbStat CbToughness)
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
            (intOptions 0 10 currentVal)
        ]


viewHpField : Int -> Int -> (Int -> msg) -> (String -> msg) -> Html msg
viewHpField currentHp maxHp onHpChange onMaxHpChange =
    label [ class "stat-row hp-row", title "HP / Max HP" ]
        [ span [ class "stat-label" ] [ text "HP" ]
        , div [ class "hp-controls" ]
            [ select
                [ class "stat-select hp-select"
                , onInput
                    (\s ->
                        String.toInt s
                            |> Maybe.withDefault 0
                            |> onHpChange
                    )
                ]
                (intOptions 0 (max 0 maxHp) currentHp)
            , span [ class "hp-divider" ] [ text "/" ]
            , input
                [ type_ "number"
                , class "hp-max-input"
                , Html.Attributes.min "0"
                , value (String.fromInt maxHp)
                , onInput onMaxHpChange
                ]
                []
            ]
        ]


viewItem : (Int -> msg) -> Int -> String -> Html msg
viewItem f index item =
    li []
        [ text (item ++ " ")
        , button [ onClick <| f index ] [ text "Remove" ]
        ]


viewInventory : (CharacterEdit -> msg) -> Character -> Html msg
viewInventory f character =
    div [ class "character-inventory" ]
        [ h3 [] [ text "Inventory" ]
        , ul [] (List.indexedMap (viewItem (f << RemoveItem)) character.inventory)
        , div []
            [ input
                [ type_ "text"
                , value character.newItem
                , onInput (f << SetNewItem)
                ]
                []
            , button [ onClick <| f SubmitItem ] [ text "Add item" ]
            ]
        ]


viewCharacter :
    { onToggle : msg
    , onRemove : msg
    , onEdit : CharacterEdit -> msg
    , currencyLabel : String
    , omenLabel : String
    , character : Character
    }
    -> Html msg
viewCharacter config =
    let
        character =
            config.character
    in
    li []
        (button
            [ class "btn-secondary"
            , onClick config.onToggle
            ]
            [ text character.name ]
            :: (if character.isOpen then
                    [ div [ class "character-stats" ]
                        [ input [ type_ "text", value character.class, onInput (config.onEdit << SetClass) ] []
                        , viewOmenSelect config.omenLabel character.omens (config.onEdit << SetOmens)
                        , viewHpField character.hp character.maxHp (config.onEdit << SetHp) (config.onEdit << SetMaxHp)
                        ]
                    , div [ class "character-currency" ]
                        [ label [ title config.currencyLabel ]
                            [ span [ class "stat-label" ] [ text config.currencyLabel ]
                            , input
                                [ type_ "number"
                                , Html.Attributes.min "0"
                                , value (String.fromInt character.currency)
                                , onInput (config.onEdit << SetCurrency)
                                ]
                                []
                            ]
                        ]
                    , viewStats config.onEdit character.stats
                    , viewInventory config.onEdit character
                    , div [ class "character-notes" ]
                        [ h3 [] [ text "Notes" ]
                        , textarea [ value character.notes, onInput (config.onEdit << SetNotes) ] []
                        ]
                    , button [ class "character-remove", onClick config.onRemove ] [ text "Remove" ]
                    ]

                else
                    []
               )
        )
