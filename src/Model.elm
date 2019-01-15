module Model exposing (Id, Item, Model, Msg(..), dnd)

import DnD exposing (Draggable)
import Maybe exposing (Maybe)
import Question exposing (Question)
import Random exposing (Seed)
import Time exposing (Posix)


{-| Id for draggable items
-}
type alias Id =
    Int


{-| record for draggable items
-}
type alias Item =
    { id : Id
    , text : String
    }


type alias Model =
    { draggable : DnD.Draggable () Item
    , randomSeed : Maybe Seed
    , question : Question
    , startTime : Maybe Posix
    }


type Msg
    = CloseWelcomeScreen
    | DropToGrid Item
    | DragMsg (DnD.Msg () Item)
    | StartApp Posix


dnd =
    DnD.init DragMsg (always DropToGrid)
