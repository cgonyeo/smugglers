module Smugglers.Auth exposing (..)

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
    { email    : String
    , password : String
    , state    : State
    , mdl      : Material.Model
    }

model : Model
model =
    { email    = ""
    , password = ""
    , state    = Input
    , mdl      = Material.model
    }

-- MESSAGES

type Msg
    = ChangeEmail String
    | ChangePassword String
    | Auth
    | AuthSuccess (List Smugglers.Data.Rum)
    | AuthError Http.Error
    | Mdl Material.Msg

-- UPDATE

loadTask : String -> String -> Task Http.Error (List Smugglers.Data.Rum)
loadTask email password =
    case Smugglers.Data.authHeaders (Smugglers.Data.User email password) of
        (Ok hdrs) ->
            Http.fromJson Smugglers.Data.decodeRum
                (Http.send Http.defaultSettings
                    { verb = "GET"
                    , headers = hdrs
                    , url = "https://api.smuggle.rs/rums"
                    , body = Http.empty
                    }
                )
        (Err err) -> Task.fail (Http.UnexpectedPayload err)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Mdl action' -> Material.update Mdl action' model
        Auth ->
            ( {model | state = Loading }, Task.perform AuthError AuthSuccess (loadTask model.email model.password))
        AuthSuccess res ->
            (model, Cmd.none)
        AuthError (Http.BadResponse 403 _) ->
            ({model | state = Error "Invalid username or password"}, Cmd.none)
        AuthError err ->
            ({model | state = Error (toString err)}, Cmd.none)
        ChangeEmail email ->
            ({model | email = email}, Cmd.none)
        ChangePassword password ->
            ({model | password = password}, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model = 
    let header = [ Html.h1 [] [ Html.text "smuggle.rs" ] ]
        form   = [ Textfield.render Mdl [0] model.mdl [ Textfield.onInput ChangeEmail
                                                      , Textfield.label "username"
                                                      , Textfield.floatingLabel
                                                      ]
                 , Html.br [] []
                 , Textfield.render Mdl [1] model.mdl [ Textfield.onInput ChangePassword
                                                      , Textfield.label "password"
                                                      , Textfield.floatingLabel
                                                      , Textfield.password
                                                      ]
                 , Html.br [] []
                 , Button.render Mdl [2] model.mdl [ Button.raised
                                                   , Button.plain
                                                   , Button.ripple
                                                   , Button.onClick Auth
                                                   ]
                                                   [ Html.text "Login"
                                                   ]
                 ]
        css = [ Options.css "margin" "auto"
              , Options.css "padding-left" "8%"
              , Options.css "padding-right" "8%"
              , Options.css "text-align" "center"
              ]
    in case model.state of
          Input     -> Options.div css
                            (header ++ form)
          Error msg -> Options.div css
                            (header ++ [ Html.p [] [Html.text msg]] ++ form)
          Loading   -> Options.div css
                            (header ++ [ Html.p [] [ Html.text "loading..." ]
                                       , Loading.spinner [ Loading.active True
                                                         , Loading.singleColor True
                                                         ]
                                       ])
