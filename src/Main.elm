-- Copyright (c) 2026 Derek Gustafson.


module Main exposing (main)

import Browser
import Character exposing (Game, feedName, gameName, newGameButtons, themeClass)
import Group exposing (Group, GroupEdit, applyEdit, displayName, encodeGroup, findGroup, groupDecoder, newGroup, updateGroup, viewGroupBody)
import Html exposing (Html, aside, button, div, footer, h1, h2, header, label, li, main_, p, section, span, text, ul)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Ports exposing (exportState, fileLoaded, log, openFile, saveState)


type Screen
    = ChooseGroup
    | ViewGroup Int


type alias Model =
    { groups : List Group
    , nextId : Int
    , screen : Screen
    , message : Maybe String
    }


getGame : Model -> Maybe Game
getGame model =
    case model.screen of
        ChooseGroup ->
            Nothing

        ViewGroup id ->
            findGroup id model.groups |> Maybe.map (\g -> g.game)


type Msg
    = NewGroup Game
    | OpenGroup Int
    | DeleteGroup Int
    | BackToStart
    | SaveToFile
    | LoadFromFile
    | FileLoaded String
    | Edit GroupEdit


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


viewGroupItem : Group -> Html Msg
viewGroupItem group =
    li [ class "group-row" ]
        [ button [ class "open-btn", onClick (OpenGroup group.id) ]
            [ text <| displayName group ++ " (" ++ gameName group.game ++ ")" ]
        , button [ class "danger-btn", onClick (DeleteGroup group.id) ]
            [ text "Delete" ]
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
            , div [ class "actions" ] <| newGameButtons NewGroup
            ]
        ]


viewSaveLoad : Maybe String -> Html Msg
viewSaveLoad message =
    div [ class "card" ]
        ([ h2 [] [ text "Save and load" ]
         , div [ class "actions" ]
            [ button [ class "btn-secondary", onClick SaveToFile ] [ text "Save to file" ]
            , button [ class "btn-secondary", onClick LoadFromFile ] [ text "Load from file" ]
            ]
         ]
            ++ (case message of
                    Just m ->
                        [ p [ class "card-note" ] [ text m ] ]

                    Nothing ->
                        []
               )
        )


viewMetaRow : String -> String -> Html msg
viewMetaRow label value =
    div [ class "meta-row" ]
        [ span [ class "meta-label" ] [ text label ]
        , span [ class "meta-value" ] [ text value ]
        ]


viewHome : Model -> Html Msg
viewHome model =
    main_ [ class "shell shell--triple" ]
        [ viewLeftRail model.groups
        , viewCenterFeed
        , viewSaveLoad model.message
        ]


viewGroup : Group -> Html Msg
viewGroup group =
    section [ class "card active-group-view" ]
        [ div [] <|
            div [ class "active-group-header" ]
                [ h2 [] [ text (gameName group.game) ]
                , button
                    [ class "btn-secondary"
                    , onClick BackToStart
                    ]
                    [ text "← All groups" ]
                ]
                :: viewGroupBody Edit group
        ]


viewFooter : Html Msg
viewFooter =
    footer []
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
    div [ class <| "app " ++ (themeClass <| getGame model) ]
        [ viewTopBar model
        , case model.screen of
            ChooseGroup ->
                viewHome model

            ViewGroup groupId ->
                main_ [ class "shell shell--single" ]
                    [ case findGroup groupId model.groups of
                        Just group ->
                            viewGroup group

                        Nothing ->
                            viewHome model
                    ]
        , viewFooter
        ]


encodeModel : Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "groups", Encode.list encodeGroup model.groups )
        , ( "nextId", Encode.int model.nextId )
        ]


persistModel : Model -> Cmd Msg
persistModel model =
    saveState (encodeModel model)


savedStateDecoder : Decode.Decoder ( List Group, Int )
savedStateDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "groups" (Decode.list groupDecoder))
        (Decode.field "nextId" Decode.int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGroup game ->
            let
                nextModel =
                    { model
                        | groups = model.groups ++ [ newGroup model.nextId game ]
                        , nextId = model.nextId + 1
                        , screen = ViewGroup model.nextId
                        , message = Nothing
                    }
            in
            ( nextModel, persistModel nextModel )

        OpenGroup id ->
            ( { model | screen = ViewGroup id, message = Nothing }, Cmd.none )

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

                newGroups =
                    List.filter (\g -> g.id /= id) model.groups

                newNextId =
                    List.foldl (\g m -> max g.id m) 0 newGroups + 1

                nextModel =
                    { groups = newGroups
                    , nextId = newNextId
                    , screen = nextScreen
                    , message = Nothing
                    }
            in
            ( nextModel, persistModel nextModel )

        BackToStart ->
            ( { model | screen = ChooseGroup, message = Nothing }, Cmd.none )

        SaveToFile ->
            ( { model | message = Nothing }, exportState (encodeModel model) )

        LoadFromFile ->
            ( { model | message = Nothing }, openFile () )

        FileLoaded raw ->
            case Decode.decodeString savedStateDecoder raw of
                Ok ( groups, _ ) ->
                    let
                        imported =
                            List.indexedMap (\i g -> { g | id = model.nextId + i }) groups

                        count =
                            List.length imported

                        nextModel =
                            { model
                                | groups = model.groups ++ imported
                                , nextId = model.nextId + count
                            }
                    in
                    ( nextModel, persistModel nextModel )

                Err _ ->
                    ( { model | message = Just "I can't understand that file." }, Cmd.none )

        Edit edit ->
            case model.screen of
                ViewGroup id ->
                    let
                        nextModel =
                            { model | groups = updateGroup id (applyEdit edit) model.groups }
                    in
                    ( nextModel, persistModel nextModel )

                ChooseGroup ->
                    ( model, Cmd.none )


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        loaded =
            Decode.decodeValue
                (Decode.oneOf
                    [ savedStateDecoder
                    , Decode.field "savedState" savedStateDecoder
                    , Decode.string
                        |> Decode.andThen
                            (\raw ->
                                case Decode.decodeString savedStateDecoder raw of
                                    Ok parsed ->
                                        Decode.succeed parsed

                                    Err _ ->
                                        Decode.fail "Invalid JSON string in localStorage"
                            )
                    ]
                )
                flags
    in
    case loaded of
        Ok data ->
            ( { groups = Tuple.first data
              , nextId = Tuple.second data
              , screen = ChooseGroup
              , message = Nothing
              }
            , Cmd.none
            )

        Err e ->
            ( { groups = []
              , nextId = 1
              , screen = ChooseGroup
              , message = Nothing
              }
            , log <| Decode.errorToString e
            )


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> fileLoaded FileLoaded
        }
