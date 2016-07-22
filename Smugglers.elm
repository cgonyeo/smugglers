import Html exposing (..)
import Html.Attributes exposing (href, class, style)
import Html.Lazy
import Html.App as App
import Platform.Cmd exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)
import String

import Navigation
import RouteUrl as Routing

import Material
import Material.Color as Color
import Material.Layout as Layout 
import Material.Helpers exposing (pure, lift, lift')
import Material.Options as Options exposing (css, when)
import Material.Scheme as Scheme
import Material.Icon as Icon

import Smugglers.Data
import Smugglers.Rums
import Smugglers.Auth
import Smugglers.Update
import Smugglers.Item

-- MODEL

type SubModel
    = AuthModel
    | RumsModel
    | UpdateModel
    | ItemModel Int

type alias Model =
    { mdl         : Material.Model
    , user        : Maybe Smugglers.Data.User
    , rums        : Maybe (List Smugglers.Data.Rum)
    , authModel   : Smugglers.Auth.Model
    , rumsModel   : Smugglers.Rums.Model
    , updateModel : Smugglers.Update.Model
    , itemModel   : Smugglers.Item.Model
    , activeModel : SubModel
    --, user       : Maybe Data.User
    }

initModel : Model
initModel =
    { mdl         = Material.model
    , user        = Nothing
    , rums        = Nothing
    , authModel   = Smugglers.Auth.model
    , rumsModel   = Smugglers.Rums.model
    , updateModel = Smugglers.Update.model
    , itemModel   = Smugglers.Item.model
    , activeModel = AuthModel
    }

-- ACTION, UPDATE

type Msg
    = Mdl          Material.Msg
    | SelectView   SubModel
    | RedirectView SubModel
    | AuthMsg      Smugglers.Auth.Msg
    | RumsMsg      Smugglers.Rums.Msg
    | UpdateMsg    Smugglers.Update.Msg
    | ItemMsg      Smugglers.Item.Msg

redirectTo : Model -> SubModel -> ( Model, Cmd Msg )
redirectTo model m = let model' = { model | activeModel = m }
                     in (model', Navigation.newUrl (urlOf model'))

newRums : Model -> List Smugglers.Data.Rum -> ( Model, Cmd Msg )
newRums model rums = 
            let rumsModel = model.rumsModel
                user      = Just (Smugglers.Data.User
                                     model.authModel.email
                                     model.authModel.password)
            in ( { model | rumsModel   = {rumsModel | rums = Just rums
                                                    , user = user
                                         }
                         , activeModel = RumsModel
                         , user        = user
                         , rums        = Just rums
                 }
               , Cmd.none
               )

update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case (model.user,action) of
        -- MDL
        (_,Mdl msg) -> Material.update Mdl msg model

        
        -- Redirects
        (_,RedirectView m) -> redirectTo model m


        -- Auth
        (Nothing,SelectView AuthModel) -> ( { model | activeModel = AuthModel }, Cmd.none)
        (Just _,SelectView AuthModel) -> redirectTo model RumsModel
        (_,AuthMsg (Smugglers.Auth.AuthSuccess rums)) -> newRums model rums
        (_,AuthMsg submsg) ->
            let (updatedAuthModel, authCmd)
                    = Smugglers.Auth.update submsg model.authModel
             in ( { model | authModel = updatedAuthModel}
                , Cmd.map AuthMsg authCmd
                )


        -- If you're not logged in, you can only see the auth page
        (Nothing,_) -> redirectTo model AuthModel

        -- From here on out, you can select anything
        (_,SelectView m) -> ( { model | activeModel = m }, Cmd.none)

        -- Rums
        (_,RumsMsg (Smugglers.Rums.Click rum)) ->
            let itemModel = Smugglers.Item.model
                itemModel' = case rum.notes of
                                (Just note) -> { itemModel | rum = rum
                                                            , note = note
                                                }
                                Nothing      -> { itemModel | rum = rum }
            in redirectTo {model | itemModel = itemModel' } (ItemModel rum.rumId)
        (_,RumsMsg Smugglers.Rums.Update) ->
            let updateModel = model.updateModel
                model' = { model | updateModel = { updateModel | user = model.user }
                         }
            in redirectTo model' UpdateModel
        (_,RumsMsg Smugglers.Rums.Logout) ->
            redirectTo initModel AuthModel
        (_,RumsMsg submsg) -> let (updatedRumsModel, rumsCmd)
                                    = Smugglers.Rums.update submsg model.rumsModel
                              in ( { model | rumsModel = updatedRumsModel}
                                 , Cmd.map RumsMsg rumsCmd
                                 )


        -- Update
        (_,UpdateMsg Smugglers.Update.Back) ->
            redirectTo model RumsModel
        (_,UpdateMsg (Smugglers.Update.UpdateSuccess rums)) ->
            let model' = { model | updateModel = Smugglers.Update.model }
            in newRums model' rums
        (_,UpdateMsg submsg) -> let (updatedUpdateModel, updateCmd)
                                       = Smugglers.Update.update submsg model.updateModel
                                in ( { model | updateModel = updatedUpdateModel}
                                   , Cmd.map UpdateMsg updateCmd
                                   )
        
        -- Item
        (_,ItemMsg Smugglers.Item.Back) ->
            redirectTo model RumsModel
        (_,ItemMsg submsg) -> let (updatedItemModel, itemCmd)
                                       = Smugglers.Item.update submsg model.itemModel
                                in ( { model | itemModel = updatedItemModel}
                                   , Cmd.map ItemMsg itemCmd
                                   )

-- VIEW

resources : Html Msg
resources = div []
                [ node "link"
                    [ Html.Attributes.attribute "rel" "stylesheet"
                    , Html.Attributes.attribute "type" "text/css"
                    , Html.Attributes.attribute "href" "https://fonts.googleapis.com/css?family=Roboto:400,300,500|Roboto+Mono|Roboto+Condensed:400,700&subset=latin,latin-ext"
                    ] []
                , node "link"
                    [ Html.Attributes.attribute "rel" "stylesheet"
                    , Html.Attributes.attribute "type" "text/css"
                    , Html.Attributes.attribute "href" "https://fonts.googleapis.com/icon?family=Material+Icons"
                    ] []
                , node "link"
                    [ Html.Attributes.attribute "rel" "stylesheet"
                    , Html.Attributes.attribute "type" "text/css"
                    , Html.Attributes.attribute "href" "https://code.getmdl.io/1.1.3/material.teal-red.min.css"
                    ] []
                ]

view : Model -> Html Msg
view model =
    let curr = case model.activeModel of
        AuthModel   -> App.map AuthMsg (Smugglers.Auth.view model.authModel)
        RumsModel   -> App.map RumsMsg (Smugglers.Rums.view model.rumsModel)
        UpdateModel -> App.map UpdateMsg (Smugglers.Update.view model.updateModel)
        ItemModel _ -> App.map ItemMsg (Smugglers.Item.view model.itemModel)
    in Layout.render Mdl model.mdl
        [ Layout.fixedHeader ]
        { header = []
        , drawer = []
        , tabs = ([],[])
        , main = [ resources, curr ]
        }


-- ROUTING


urlOf : Model -> String
urlOf model = 
    case model.activeModel of
        AuthModel -> "#auth"
        RumsModel -> "#rums"
        UpdateModel -> "#update"
        ItemModel i -> "#item-" ++ (toString i)

delta2url : Model -> Model -> Maybe Routing.UrlChange
delta2url model1 model2 = 
  if model1.activeModel /= model2.activeModel then 
    { entry = Routing.NewEntry
    , url = urlOf model2
    } |> Just
  else
    Nothing


location2messages : Navigation.Location -> List Msg
location2messages location = 
  [ case String.dropLeft 1 location.hash of
      "auth" -> SelectView AuthModel
      "rums" -> SelectView RumsModel
      "update" -> SelectView UpdateModel
      s      -> case String.split "-" s of
                    ["item",str] ->
                        case String.toInt str of
                            (Ok i) -> SelectView (ItemModel i)
                            (Err _) -> RedirectView AuthModel
                    _ -> RedirectView AuthModel
  ]

-- APP


main : Program Never
main =
  Routing.program 
    { delta2url = delta2url
    , location2messages = location2messages
    , init = 
        ( { initModel
          | mdl = Layout.setTabsWidth 1384 initModel.mdl
          }
          , Layout.sub0 Mdl 
        )
    , view = view
    , subscriptions = .mdl >> Layout.subs Mdl
    , update = update
    }
