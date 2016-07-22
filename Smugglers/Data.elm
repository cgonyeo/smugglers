module Smugglers.Data exposing (..)

import Json.Decode as Decode exposing ((:=))
import Base64

type alias User =
    { email    : String
    , password : String
    }

type alias Note =
    { rating   : Float
    , color    : String
    , smells   : String
    , taste    : String
    , thoughts : String
    }

type alias Rum =
    { rumId     : Int
    , country   : String
    , name      : String
    , price     : Float
    , immortal  : Bool
    , signer    : Maybe String
    , requested : Maybe String
    , notes     : Maybe Note
    }

nullOr : Decode.Decoder a -> Decode.Decoder (Maybe a)
nullOr decoder =
    Decode.oneOf
      [ Decode.null Nothing
      , Decode.map Just decoder
      ]

decodeNote : Decode.Decoder Note
decodeNote = Decode.object5 Note
                ("rating"   := Decode.float)
                ("color"    := Decode.string)
                ("smells"   := Decode.string)
                ("taste"    := Decode.string)
                ("thoughts" := Decode.string)

decodeRum : Decode.Decoder (List Rum)
decodeRum = Decode.list (Decode.object8 Rum
            ("rumId"     := Decode.int)
            ("country"   := Decode.string)
            ("name"      := Decode.string)
            ("price"     := Decode.float)
            ("immortal"  := Decode.bool)
            ("signer"    := nullOr Decode.string)
            ("requested" := nullOr Decode.string)
            ("notes"     := nullOr decodeNote)
        )

authHeaders : User -> Result String (List (String,String))
authHeaders user = case Base64.encode (user.email ++ ":" ++ user.password) of
                        (Ok encodedCreds) -> Ok [ ( "Authorization"
                                                  , "Basic " ++ encodedCreds
                                                  )
                                                ]
                        (Err err) -> Err ("error encoding creds: " ++ err)
