module Smugglers.Item exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Platform.Cmd exposing (Cmd)
import String

import Material.Button as Button
import Material.Table as Table
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Options as Options exposing (Style)
import Material.Helpers exposing (map1st, map2nd)
import Material

import Http
import Task exposing (Task)

import Smugglers.Data

-- MODEL

type alias Model =
    { rum  : Smugglers.Data.Rum
    , note : Smugglers.Data.Note
    , mdl  : Material.Model
    }

model : Model
model =
    { rum = Smugglers.Data.Rum -1 "" "" -1 False Nothing Nothing Nothing
    , note = Smugglers.Data.Note -1 "" "" "" ""
    , mdl = Material.model
    }

-- MESSAGES

type Msg
    = Mdl Material.Msg
    | ChangeRating Float
    | ChangeColor String
    | ChangeSmells String
    | ChangeTaste String
    | ChangeThoughts String
    | UpdateNote
    | UpdateNoteSuccess
    | UpdateNoteError Http.Error
    | Request
    | RequestSuccess
    | RequestError Http.Error
    | Back

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    let note = model.note
    in case action of
        Mdl action' -> Material.update Mdl action' model
        ChangeRating r ->
            ( { model | note = { note | rating = r } }, Cmd.none )
        ChangeColor c ->
            ( { model | note = { note | color = c } }, Cmd.none )
        ChangeSmells s ->
            ( { model | note = { note | smells = s } }, Cmd.none )
        ChangeTaste t ->
            ( { model | note = { note | taste = t } }, Cmd.none )
        ChangeThoughts t ->
            ( { model | note = { note | thoughts = t } }, Cmd.none )
        _ -> (model, Cmd.none)

-- VIEW

isJust : Maybe a -> Bool
isJust x = case x of
                (Just _) -> True
                Nothing  -> False

view : Model -> Html Msg
view model = 
    Options.div []
        [ Options.div [Options.css "text-align" "center"]
            [ h1 [] [ text "smuggle.rs" ]
            , Button.render Mdl [0] model.mdl [ Button.flat
                                              , Button.plain
                                              , Button.ripple
                                              , Button.onClick Back
                                              ]
                                              [ Html.text "Back to Rums"
                                              ]

            ]
        , Grid.grid []
            [ Grid.cell [Grid.size Grid.All 6]
                [ h5 [] [text "Name"]
                , text model.rum.name
                , h5 [] [text "Country"]
                , text model.rum.country
                , h5 [] [text "Price"]
                , text ("$" ++ (toString model.rum.price))
                , h5 [] [text "Immortal"]
                , text (if model.rum.immortal then "Yes" else "No")
                , h5 [] [text "Requested"]
                , text (case model.rum.requested of
                    (Just req) -> "Requested on " ++ req
                    Nothing    -> "Not requested")
                , h5 [] [text "Signed Off"]
                , text (case model.rum.signer of
                    (Just sig) -> "Signed off by " ++ sig
                    Nothing    -> "Not signed off")
                ]
            , Grid.cell [Grid.size Grid.All 6]
                [
                ]
            ]
        ]
