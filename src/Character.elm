-- Copyright (c) 2026 Derek Gustafson.


module Character exposing (Character, CharacterEdit, Game, applyEdit, characterDecoder, counterName, encodeCharacter, encodeGame, feedName, gameDecoder, gameName, newCharacter, newGameButtons, themeClass, viewCharacter)

import Html exposing (Html, button, details, div, h3, input, label, li, option, select, span, summary, text, textarea, ul)
import Html.Attributes exposing (class, selected, title, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra exposing (removeAt)


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


encodeGame : Game -> Encode.Value
encodeGame =
    Encode.string << gameName


gameDecoder : Decode.Decoder Game
gameDecoder =
    Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "Mork Borg" ->
                        Decode.succeed MorkBorg

                    "Cy_Borg" ->
                        Decode.succeed CyBorg

                    _ ->
                        Decode.fail ("Unknown game: " ++ s)
            )


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
            "¤"


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


encodeMbStats : MbStats -> Encode.Value
encodeMbStats s =
    Encode.object
        [ ( "agility", Encode.int s.agility )
        , ( "presence", Encode.int s.presence )
        , ( "strength", Encode.int s.strength )
        , ( "toughness", Encode.int s.toughness )
        ]


mbStatsDecoder : Decode.Decoder MbStats
mbStatsDecoder =
    Decode.map4 MbStats
        (Decode.field "agility" Decode.int)
        (Decode.field "presence" Decode.int)
        (Decode.field "strength" Decode.int)
        (Decode.field "toughness" Decode.int)


type alias CbStats =
    { agility : Int
    , knowledge : Int
    , presence : Int
    , strength : Int
    , toughness : Int
    }


encodeCbStats : CbStats -> Encode.Value
encodeCbStats s =
    Encode.object
        [ ( "agility", Encode.int s.agility )
        , ( "knowledge", Encode.int s.knowledge )
        , ( "presence", Encode.int s.presence )
        , ( "strength", Encode.int s.strength )
        , ( "toughness", Encode.int s.toughness )
        ]


cbStatsDecoder : Decode.Decoder CbStats
cbStatsDecoder =
    Decode.map5 CbStats
        (Decode.field "agility" Decode.int)
        (Decode.field "knowledge" Decode.int)
        (Decode.field "presence" Decode.int)
        (Decode.field "strength" Decode.int)
        (Decode.field "toughness" Decode.int)


type CharacterStats
    = MbCharacterStats MbStats
    | CbCharacterStats CbStats


encodeStats : CharacterStats -> Encode.Value
encodeStats stats =
    case stats of
        MbCharacterStats mb ->
            Encode.object
                [ ( "type", Encode.string "mb" )
                , ( "stats", encodeMbStats mb )
                ]

        CbCharacterStats cb ->
            Encode.object
                [ ( "type", Encode.string "cb" )
                , ( "stats", encodeCbStats cb )
                ]


statsDecoder : Decode.Decoder CharacterStats
statsDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\t ->
                case t of
                    "mb" ->
                        Decode.map MbCharacterStats (Decode.field "stats" mbStatsDecoder)

                    "cb" ->
                        Decode.map CbCharacterStats (Decode.field "stats" cbStatsDecoder)

                    _ ->
                        Decode.fail ("Unknown stats type: " ++ t)
            )


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
    { game : Game
    , name : String
    , class : String
    , hp : Int
    , maxHp : Int
    , currency : Int
    , omens : Int
    , stats : CharacterStats
    , inventory : List String
    , newItem : String
    , notes : String
    }


encodeCharacter : Character -> Encode.Value
encodeCharacter c =
    Encode.object
        [ ( "game", encodeGame c.game )
        , ( "name", Encode.string c.name )
        , ( "class", Encode.string c.class )
        , ( "hp", Encode.int c.hp )
        , ( "maxHp", Encode.int c.maxHp )
        , ( "currency", Encode.int c.currency )
        , ( "omens", Encode.int c.omens )
        , ( "stats", encodeStats c.stats )
        , ( "inventory", Encode.list Encode.string c.inventory )
        , ( "newItem", Encode.string c.newItem )
        , ( "notes", Encode.string c.notes )
        ]


andMap : Decode.Decoder a -> Decode.Decoder (a -> b) -> Decode.Decoder b
andMap argFunc func =
    Decode.map2 (<|) func argFunc


characterDecoder : Decode.Decoder Character
characterDecoder =
    Decode.succeed Character
        |> andMap (Decode.field "game" gameDecoder)
        |> andMap (Decode.field "name" Decode.string)
        |> andMap (Decode.field "class" Decode.string)
        |> andMap (Decode.field "hp" Decode.int)
        |> andMap (Decode.field "maxHp" Decode.int)
        |> andMap (Decode.field "currency" Decode.int)
        |> andMap (Decode.field "omens" Decode.int)
        |> andMap (Decode.field "stats" statsDecoder)
        |> andMap (Decode.field "inventory" (Decode.list Decode.string))
        |> andMap (Decode.field "newItem" Decode.string)
        |> andMap (Decode.field "notes" Decode.string)


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
    { game = game
    , name = name
    , class = "Classless"
    , hp = 1
    , maxHp = 1
    , currency = 0
    , omens = 0
    , stats = defaultStats game
    , inventory = []
    , newItem = ""
    , notes = ""
    }


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


statOptions : Int -> Int -> Int -> List (Html msg)
statOptions lb ub currentVal =
    List.range lb ub
        |> List.map
            (\n ->
                let
                    nStr =
                        (if n >= 0 then
                            "+"

                         else
                            ""
                        )
                            ++ String.fromInt n
                in
                option
                    [ value nStr
                    , selected (n == currentVal)
                    ]
                    [ text nStr ]
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
    { onRemove : msg
    , onEdit : CharacterEdit -> msg
    , character : Character
    }
    -> Html msg
viewCharacter config =
    let
        character =
            config.character
    in
    li []
        [ details
            []
            [ summary []
                [ text character.name ]
            , div [ class "character-stats" ]
                [ input [ type_ "text", value character.class, onInput (config.onEdit << SetClass) ] []
                , viewOmenSelect (omenName config.character.game) character.omens (config.onEdit << SetOmens)
                , viewHpField character.hp character.maxHp (config.onEdit << SetHp) (config.onEdit << SetMaxHp)
                ]
            , div [ class "character-currency" ]
                [ label [ title <| currencyName config.character.game ]
                    [ span [ class "stat-label" ] [ text <| currencyName config.character.game ]
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
        ]
