module Smugglers.Update exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Platform.Cmd exposing (Cmd)
import String

import Material.Button as Button
import Material.Layout as Layout
import Material.Spinner as Loading
import Material.Progress as Loading
import Material.Textfield as Textfield
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Options as Options exposing (Style)
import Material.Helpers exposing (map1st, map2nd)
import Material

import Http
import Task exposing (Task)

import Smugglers.Data

-- MODEL

type State = Input
           | Loading
           | Error String

type alias Model =
    { user  : Maybe Smugglers.Data.User
    , state : State
    , mdl   : Material.Model
    }

model : Model
model =
    { user  = Nothing
    , state = Input
    , mdl   = Material.model
    }

-- MESSAGES

type Msg
    = Update
    | UpdateSuccess (List Smugglers.Data.Rum)
    | UpdateError Http.Error
    | Mdl Material.Msg
    | Back

-- UPDATE

loadTask : Smugglers.Data.User -> Task Http.Error (List Smugglers.Data.Rum)
loadTask user =
    case Smugglers.Data.authHeaders user of
        (Ok hdrs) ->
            Http.fromJson Smugglers.Data.decodeRum
                (Http.send Http.defaultSettings
                    { verb = "POST"
                    , headers = hdrs
                    , url = "https://api.smuggle.rs/update-rums"
                    , body = Http.empty
                    }
                )
        (Err err) -> Task.fail (Http.UnexpectedPayload err)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Mdl action' -> Material.update Mdl action' model
        Update -> case model.user of
                    Just user -> ( {model | state = Loading }
                                 , Task.perform UpdateError UpdateSuccess (loadTask user)
                                 )
                    Nothing -> (model, Cmd.none)
        UpdateSuccess res ->
            (model, Cmd.none)
        UpdateError (Http.BadResponse 403 _) ->
            ({model | state = Error "Invalid username or password (this shouldn't happen!)"}, Cmd.none)
        UpdateError err ->
            ({model | state = Error (toString err)}, Cmd.none)
        Back ->
            (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
    let header = [ Html.h1 [] [ Html.text "smuggle.rs" ] ]
        body   = [ Button.render Mdl [0] model.mdl [ Button.flat
                                                   , Button.plain
                                                   , Button.ripple
                                                   , Button.onClick Back
                                                   ]
                                                   [ Html.text "Back to Rums"
                                                   ]
                 , let blurb =  "This button will make smuggle.rs log in as "
                             ++ "you to sc.bevager.com and look for any "
                             ++ "updates to your data. You'll need to press "
                             ++ "this to get any updates for rums being signed "
                             ++ "off on."
                   in Grid.grid [] [Grid.cell [ Grid.size Grid.All 4
                                              , Grid.offset Grid.All 4
                                              ] [text blurb]]
                 , Button.render Mdl [1] model.mdl [ Button.raised
                                                   , Button.plain
                                                   , Button.ripple
                                                   , Button.onClick Update
                                                   ]
                                                   [ Html.text "Update"
                                                   ]
                 ]
        css = [ Options.css "margin" "auto"
              , Options.css "padding-left" "8%"
              , Options.css "padding-right" "8%"
              , Options.css "text-align" "center"
              ]
    in case model.state of
          Input     -> Options.div css
                            (header ++ body)
          Error msg -> Options.div css
                            (header ++ [ Html.p [] [Html.text msg]] ++ body)
          Loading   -> Options.div css
                            (header ++ [ Html.p [] [Html.text "updating..."]
                                       , Html.p [] [Html.text "This usually takes about 10 seconds."]
                                       , Loading.spinner [ Loading.active True
                                                         , Loading.singleColor True
                                                         ]
                                       ])
