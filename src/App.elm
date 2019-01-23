module Main exposing (main)

import Browser exposing (document)
import Model exposing (Model, Msg)
import Update exposing (subscriptions, update)
import View exposing (view)


{-| TO DO
-- Change PerfectFlood to have many droppables and use Dropped Int Metadata formatß
-}
main : Program () Model Msg
main =
    Browser.document
        { init = Update.init
        , update = Update.update
        , view = View.view
        , subscriptions = Update.subscriptions
        }
