-- Copyright (c) 2026 Derek Gustafson.


module Main exposing (main)

import Browser
import Html exposing (Html, aside, button, div, footer, h1, h2, h3, header, input, label, li, main_, option, p, section, select, span, text, textarea, ul)
import Html.Attributes exposing (class, disabled, selected, type_, value)
import Html.Events exposing (onClick, onInput)


type Game
    = MorkBorg
    | CyBorg


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
    , stats : CharacterStats
    }


type alias Group =
    { id : Int
    , game : Game
    , name : String
    , silver : Int
    , characters : List Character
    , inventory : List String
    , notes : String
    , misery : Int
    }


type Screen
    = ChooseGroup
    | ViewGroup Int


type alias Model =
    { groups : List Group
    , nextId : Int
    , screen : Screen
    , newItem : String
    , newCharacter : String
    }


type GroupEdit
    = SetName String
    | SetSilver String
    | SetNotes String
    | RemoveItem Int
    | AdjustMisery Int
    | RenameCharacter Int String
    | RemoveCharacter Int
    | SetMbStat Int MbStatField Int
    | SetCbStat Int CbStatField Int
    | AdjustMbStat Int MbStatField Int
    | AdjustCbStat Int CbStatField Int


type Msg
    = NewGroup Game
    | OpenGroup Int
    | DeleteGroup Int
    | BackToStart
    | SetNewItem String
    | SubmitItem
    | SetNewCharacter String
    | SubmitCharacter
    | Edit GroupEdit


maxMisery : Int
maxMisery =
    7


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


newGroup : Int -> Game -> Group
newGroup id game =
    { id = id
    , game = game
    , name = "Party " ++ String.fromInt id
    , silver = 0
    , characters = []
    , inventory = []
    , notes = ""
    , misery = 0
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


getGame : Model -> Maybe Game
getGame model =
    case model.screen of
        ChooseGroup ->
            Nothing

        ViewGroup id ->
            findGroup id model.groups |> Maybe.map (\g -> g.game)


updateGroup : Int -> (Group -> Group) -> List Group -> List Group
updateGroup id f =
    List.map
        (\g ->
            if g.id == id then
                f g

            else
                g
        )


removeAt : Int -> List a -> List a
removeAt index items =
    List.indexedMap Tuple.pair items
        |> List.filter (\( i, _ ) -> i /= index)
        |> List.map Tuple.second


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index f =
    List.indexedMap
        (\i x ->
            if i == index then
                f x

            else
                x
        )


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


applyEdit : GroupEdit -> Group -> Group
applyEdit edit group =
    case edit of
        SetName name ->
            { group | name = name }

        SetSilver s ->
            { group | silver = max 0 (Maybe.withDefault 0 (String.toInt s)) }

        SetNotes notes ->
            { group | notes = notes }

        RemoveItem index ->
            { group | inventory = removeAt index group.inventory }

        AdjustMisery delta ->
            { group | misery = clamp 0 maxMisery (group.misery + delta) }

        RenameCharacter index name ->
            { group
                | characters =
                    updateAt index (\c -> { c | name = name }) group.characters
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

        AdjustMbStat charIndex field delta ->
            { group
                | characters =
                    updateAt charIndex
                        (\c ->
                            case c.stats of
                                MbCharacterStats mb ->
                                    { c | stats = MbCharacterStats (adjustMbStatRecord field delta mb) }

                                other ->
                                    { c | stats = other }
                        )
                        group.characters
            }

        AdjustCbStat charIndex field delta ->
            { group
                | characters =
                    updateAt charIndex
                        (\c ->
                            case c.stats of
                                CbCharacterStats cb ->
                                    { c | stats = CbCharacterStats (adjustCbStatRecord field delta cb) }

                                other ->
                                    { c | stats = other }
                        )
                        group.characters
            }


viewGroupItem : Group -> Html Msg
viewGroupItem group =
    li [ class "group-row" ]
        [ button [ class "open-btn", onClick (OpenGroup group.id) ]
            [ text <| displayName group ++ " (" ++ gameName group.game ++ ")" ]
        , button [ class "danger-btn", onClick (DeleteGroup group.id) ]
            [ text "Delete" ]
        ]


viewTopBar : Model -> Html Msg
viewTopBar model =
    header [ class "topbar" ]
        [ div [ class "brand" ]
            [ span [ class "logo-badge" ]
                [ text "S" ]
            , h1 [] [ text "Scvmbook" ]
            ]
        , span [ class "topbar-tag" ] [ getGame model |> feedName |> text ]
        ]


viewLeftRail : List Group -> Html Msg
viewLeftRail groups =
    aside []
        [ section [ class "card" ]
            [ h2 [] [ text "Existing groups" ]
            , ul [ class "group-list" ]
                (if List.isEmpty groups then
                    [ li [ class "group-empty" ] [ text "No groups yet." ] ]

                 else
                    List.map viewGroupItem groups
                )
            ]
        ]


viewCenterFeed : Html Msg
viewCenterFeed =
    section []
        [ div [ class "card" ]
            [ h2 [] [ text "New group" ]
            , div [ class "actions" ]
                [ button
                    [ onClick (NewGroup MorkBorg) ]
                    [ text "New Mork Borg group" ]
                , button
                    [ class "btn-secondary"
                    , onClick (NewGroup CyBorg)
                    ]
                    [ text "New Cy_Borg group" ]
                ]
            ]
        ]


viewMetaRow : String -> String -> Html msg
viewMetaRow label value =
    div [ class "meta-row" ]
        [ span [ class "meta-label" ] [ text label ]
        , span [ class "meta-value" ] [ text value ]
        ]


viewHome : Model -> Html Msg
viewHome model =
    main_ [ class "shell shell--double" ]
        [ viewLeftRail model.groups
        , viewCenterFeed
        ]


viewItem : Int -> String -> Html Msg
viewItem index item =
    li []
        [ text (item ++ " ")
        , button [ onClick (Edit (RemoveItem index)) ] [ text "Remove" ]
        ]


viewInventory : String -> List String -> Html Msg
viewInventory newItem items =
    div []
        [ h3 [] [ text "Inventory" ]
        , if List.isEmpty items then
            p [] [ text "Nothing yet." ]

          else
            ul [] (List.indexedMap viewItem items)
        , div []
            [ input [ type_ "text", value newItem, onInput SetNewItem ] []
            , button [ onClick SubmitItem ] [ text "Add item" ]
            ]
        ]


viewGroup : Model -> Group -> Html Msg
viewGroup model group =
    div []
        [ div [ class "active-group-header" ]
            [ h2 [] [ text (gameName group.game) ]
            , button
                [ class "btn-secondary"
                , onClick BackToStart
                ]
                [ text "← All groups" ]
            ]
        , div []
            [ label []
                [ text "Party name "
                , input [ type_ "text", value group.name, onInput (Edit << SetName) ] []
                ]
            ]
        , div []
            [ label []
                [ text "Silver "
                , input
                    [ type_ "number"
                    , value (String.fromInt group.silver)
                    , onInput (Edit << SetSilver)
                    ]
                    []
                ]
            ]
        , viewMisery group
        , viewCharacters model.newCharacter group.characters
        , viewInventory model.newItem group.inventory
        , div []
            [ h3 [] [ text "Notes" ]
            , textarea [ value group.notes, onInput (Edit << SetNotes) ] []
            ]
        ]


viewMisery : Group -> Html Msg
viewMisery group =
    div []
        [ text (counterName group.game ++ ": ")
        , text (" " ++ String.fromInt group.misery ++ " / " ++ String.fromInt maxMisery ++ " ")
        , button
            [ onClick (Edit (AdjustMisery 1))
            , disabled (group.misery >= maxMisery)
            ]
            [ text "+" ]
        ]


viewCharacters : String -> List Character -> Html Msg
viewCharacters newCharacter characters =
    div []
        [ h3 [] [ text "Characters" ]
        , if List.isEmpty characters then
            p [] [ text "No characters yet." ]

          else
            ul [] (List.indexedMap viewCharacter characters)
        , div []
            [ input [ type_ "text", value newCharacter, onInput SetNewCharacter ] []
            , button [ onClick SubmitCharacter ] [ text "Add character" ]
            ]
        ]


statOptions : Int -> List (Html msg)
statOptions currentVal =
    List.range -3 3
        |> List.map
            (\n ->
                option
                    [ value (String.fromInt n)
                    , selected (n == currentVal)
                    ]
                    [ text (String.fromInt n) ]
            )


viewStatSelect : String -> Int -> (Int -> Msg) -> Html Msg
viewStatSelect statLabel currentVal toMsg =
    label [ class "stat-row" ]
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
            (statOptions currentVal)
        ]


viewStats : Int -> CharacterStats -> Html Msg
viewStats charIndex stats =
    div [ class "character-stats" ]
        (case stats of
            MbCharacterStats mb ->
                [ viewStatSelect "Agility" mb.agility (\n -> Edit (SetMbStat charIndex MbAgility n))
                , viewStatSelect "Presence" mb.presence (\n -> Edit (SetMbStat charIndex MbPresence n))
                , viewStatSelect "Strength" mb.strength (\n -> Edit (SetMbStat charIndex MbStrength n))
                , viewStatSelect "Toughness" mb.toughness (\n -> Edit (SetMbStat charIndex MbToughness n))
                ]

            CbCharacterStats cb ->
                [ viewStatSelect "Agility" cb.agility (\n -> Edit (SetCbStat charIndex CbAgility n))
                , viewStatSelect "Knowledge" cb.knowledge (\n -> Edit (SetCbStat charIndex CbKnowledge n))
                , viewStatSelect "Presence" cb.presence (\n -> Edit (SetCbStat charIndex CbPresence n))
                , viewStatSelect "Strength" cb.strength (\n -> Edit (SetCbStat charIndex CbStrength n))
                , viewStatSelect "Toughness" cb.toughness (\n -> Edit (SetCbStat charIndex CbToughness n))
                ]
        )


viewCharacter : Int -> Character -> Html Msg
viewCharacter index character =
    li []
        [ div []
            [ input
                [ type_ "text"
                , value character.name
                , onInput (Edit << RenameCharacter index)
                ]
                []
            , button [ onClick (Edit (RemoveCharacter index)) ] [ text "Remove" ]
            ]
        , viewStats index character.stats
        ]


themeClass : Model -> String
themeClass model =
    case getGame model of
        Nothing ->
            "theme-base"

        Just game ->
            case game of
                MorkBorg ->
                    "theme-mb"

                CyBorg ->
                    "theme-cb"


viewFooter : Html Msg
viewFooter =
    footer [ class "footer" ]
        [ div [ class "footer-inner" ]
            [ p [ class "footer-license" ]
                [ text "Scvmbook is an independent production by Derek Gustafson and is not affiliated with Ockult Örtmästare Games or Stockholm Kartell. It is published under the MÖRK BORG and CY_BORG Third Party Licenses." ]
            , p [ class "footer-copy" ]
                [ text "Scvmbook IS ©2026 Derek Gustafson. " ]
            , p [ class "footer-copy" ]
                [ text "MÖRK BORG is copyright Ockult Örtmästare Games and Stockholm Kartell." ]
            , p [ class "footer-copy" ]
                [ text "CY_BORG IS ©2022 STOCKHOLM KARTELL. " ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class <| "app " ++ themeClass model ]
        [ viewTopBar model
        , case model.screen of
            ChooseGroup ->
                viewHome model

            ViewGroup groupId ->
                main_ [ class "shell shell--single" ]
                    [ case findGroup groupId model.groups of
                        Just group ->
                            section [ class "card active-group-view" ]
                                [ viewGroup model group ]

                        Nothing ->
                            viewHome model
                    ]
        , viewFooter
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGroup game ->
            ( { model
                | groups = model.groups ++ [ newGroup model.nextId game ]
                , nextId = model.nextId + 1
                , screen = ViewGroup model.nextId
                , newItem = ""
                , newCharacter = ""
              }
            , Cmd.none
            )

        OpenGroup id ->
            ( { model | screen = ViewGroup id, newItem = "", newCharacter = "" }, Cmd.none )

        DeleteGroup id ->
            let
                nextScreen =
                    case model.screen of
                        ViewGroup activeId ->
                            if activeId == id then
                                ChooseGroup

                            else
                                model.screen

                        ChooseGroup ->
                            ChooseGroup
            in
            ( { model
                | groups = List.filter (\g -> g.id /= id) model.groups
                , screen = nextScreen
              }
            , Cmd.none
            )

        BackToStart ->
            ( { model | screen = ChooseGroup, newItem = "", newCharacter = "" }, Cmd.none )

        SetNewItem item ->
            ( { model | newItem = item }, Cmd.none )

        SubmitItem ->
            case model.screen of
                ViewGroup id ->
                    let
                        item =
                            String.trim model.newItem
                    in
                    if String.isEmpty item then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | groups =
                                updateGroup id
                                    (\g -> { g | inventory = g.inventory ++ [ item ] })
                                    model.groups
                            , newItem = ""
                          }
                        , Cmd.none
                        )

                ChooseGroup ->
                    ( model, Cmd.none )

        SetNewCharacter name ->
            ( { model | newCharacter = name }, Cmd.none )

        SubmitCharacter ->
            case model.screen of
                ViewGroup id ->
                    let
                        name =
                            String.trim model.newCharacter
                    in
                    if String.isEmpty name then
                        ( model, Cmd.none )

                    else
                        case findGroup id model.groups of
                            Just group ->
                                ( { model
                                    | groups =
                                        updateGroup id
                                            (\g ->
                                                { g
                                                    | characters =
                                                        { name = name, stats = defaultStats group.game }
                                                            :: g.characters
                                                }
                                            )
                                            model.groups
                                    , newCharacter = ""
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( model, Cmd.none )

                ChooseGroup ->
                    ( model, Cmd.none )

        Edit edit ->
            case model.screen of
                ViewGroup id ->
                    ( { model | groups = updateGroup id (applyEdit edit) model.groups }
                    , Cmd.none
                    )

                ChooseGroup ->
                    ( model, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { groups = [], nextId = 1, screen = ChooseGroup, newItem = "", newCharacter = "" }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
