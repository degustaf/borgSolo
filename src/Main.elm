-- Copyright (c) 2026 Derek Gustafson.


module Main exposing (main)

import Browser
import Html exposing (Html, aside, button, div, h1, h2, h3, header, input, label, li, main_, p, section, span, text, textarea, ul)
import Html.Attributes exposing (class, disabled, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type Game
    = MorkBorg
    | CyBorg


type alias Character =
    { name : String
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
        , div [ onSubmit SubmitItem ]
            [ input [ type_ "text", value newItem, onInput SetNewItem ] []
            , button [ type_ "submit" ] [ text "Add item" ]
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
        , div [ onSubmit SubmitCharacter ]
            [ input [ type_ "text", value newCharacter, onInput SetNewCharacter ] []
            , button [ type_ "submit" ] [ text "Add character" ]
            ]
        ]


viewCharacter : Int -> Character -> Html Msg
viewCharacter index character =
    li []
        [ input
            [ type_ "text"
            , value character.name
            , onInput (Edit << RenameCharacter index)
            ]
            []
        , button [ onClick (Edit (RemoveCharacter index)) ] [ text "Remove" ]
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
                        ( { model
                            | groups =
                                updateGroup id
                                    (\g -> { g | characters = g.characters ++ [ { name = name } ] })
                                    model.groups
                            , newCharacter = ""
                          }
                        , Cmd.none
                        )

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
