module Smugglers.Rums exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import String

import Material.Button as Button
import Material.Table as Table
import Material.Grid as Grid
import Material.Icon as Icon
import Material.Options as Options exposing (Style)
import Material.Helpers exposing (map1st, map2nd)
import Material

import Base64
import Http
import Task exposing (Task)
import Json.Decode as Decode exposing ((:=))

import Smugglers.Data

-- MODEL

type alias Model =
    { user : Maybe Smugglers.Data.User
    , rums : Maybe (List Smugglers.Data.Rum)
    , mdl  : Material.Model
    }

model : Model
model =
    { user = Nothing
    , rums = Nothing
    , mdl  = Material.model
    }

-- MESSAGES

type Msg
    = Click (Smugglers.Data.Rum)
    | Mdl Material.Msg
    | Logout
    | Update

-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Mdl action' -> Material.update Mdl action' model
        Click r ->
            (model, Cmd.none)
        _ -> (model, Cmd.none)

-- VIEW

isJust : Maybe a -> Bool
isJust x = case x of
                (Just _) -> True
                Nothing  -> False

view : Model -> Html Msg
view model = 
    case model.rums of
        (Just rs) -> Options.div [] (
            [ Options.div [Options.css "text-align" "center"]
                [ h1 [] [text "smuggle.rs"]
                , case model.user of
                             (Just user') -> text user'.email
                             Nothing      -> text ""
                , br [] []
                , let rums = "Rums: " ++ (toString (List.length (List.filter (\r -> isJust r.requested) rs)))
                      immortals = "Immortals: " ++ (toString (List.length (List.filter (\r -> isJust r.requested && r.immortal) rs)))
                  in text (rums ++ " | " ++ immortals)
                , br [] []
                , Button.render Mdl [0] model.mdl [ Button.raised
                                                  , Button.plain
                                                  , Button.ripple
                                                  , Options.css "margin-right" "1em"
                                                  , Options.css "margin-top" "1em"
                                                  , Button.onClick Update
                                                  ]
                                                  [ text "Update"
                                                  ]
                , Button.render Mdl [1] model.mdl [ Button.raised
                                                  , Button.plain
                                                  , Button.ripple
                                                  , Options.css "margin-left" "1em"
                                                  , Options.css "margin-top" "1em"
                                                  , Button.onClick Logout
                                                  ]
                                                  [ text "Logout"
                                                  ]
                ]
            , Grid.grid [Grid.maxWidth "80%", Options.css "padding" "0"]
                [ Grid.cell [ Grid.size Grid.All 1]
                    [ h6 [] [ text "Requested" ] ]
                , Grid.cell [ Grid.size Grid.All 1]
                    [ h6 [] [ text "Immortal" ] ]
                , Grid.cell [ Grid.size Grid.All 1]
                    [ h6 [] [ text "Price" ] ]
                , Grid.cell [ Grid.size Grid.All 1]
                    [ h6 [] [ text "Rating" ] ]
                , Grid.cell [ Grid.size Grid.All 2]
                    [ h6 [] [ text "Country" ] ]
                , Grid.cell [ Grid.size Grid.All 6]
                    [ h6 [] [ text "Name" ] ]
                ]
            , Html.hr [style [("margin","0")]] []
            ] ++ (List.intersperse (Html.hr [style [("margin","0")]] [])
              (rs |> List.map (\r ->
                Grid.grid [Grid.maxWidth "80%", Options.css "padding" "0"]
                    [ Grid.cell [ Grid.size Grid.All 1]
                        (case (r.requested,r.signer) of
                                          (Just _,Nothing) -> [Icon.i "cached"]
                                          (Just _,Just _)  -> [Icon.i "done"]
                                          (_,_)            -> []
                                      )
                    , Grid.cell [ Grid.size Grid.All 1]
                        ( if r.immortal
                                     then [Icon.i "all_inclusive"]
                                     else []
                               )
                    , Grid.cell [ Grid.size Grid.All 1]
                        [ text ("$" ++ (toString r.price)) ]
                    , Grid.cell [ Grid.size Grid.All 1]
                        ( case r.notes of
                                    Just n  -> [text (toString n.rating)]
                                    Nothing -> []
                               )
                    , Grid.cell [ Grid.size Grid.All 2]
                        [ text r.country ]
                    , Grid.cell [ Grid.size Grid.All 6]
                        [ a [href ("#item-"++toString r.rumId),onClick (Click r)] [text r.name] ]
                    ]
                    )
                )))
        Nothing -> Html.div [] [ Html.text "weird, there doesn't appear to be anything here" ]
