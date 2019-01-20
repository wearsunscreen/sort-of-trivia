module Model exposing (Model, Msg(..), dnd)

import DnD exposing (Draggable)
import Maybe exposing (Maybe)
import Question exposing (Choice, Question)
import Random exposing (Seed)
import Time exposing (Posix)


type alias Model =
    { answers : List Choice
    , draggable : DnD.Draggable () Choice
    , randomSeed : Seed
    , question : Question
    , startTime : Maybe Posix
    }


type Msg
    = CloseWelcomeScreen
    | DropToGrid Choice
    | DragMsg (DnD.Msg () Choice)
    | NextQuestion
    | StartApp Posix


dnd =
    DnD.init DragMsg (always DropToGrid)
