module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (Position)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (height, id, width, x, y)
import Msgs exposing (Msg)
import Components exposing (Entity(..), Component(..))
import Shape exposing (..)
import DraggableSystem exposing (..)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Drag =
    { startPos : Position
    , currentPos : Position
    }


type alias Model =
    { drag : Maybe Drag
    , entities : List Entity
    }


box1 : Entity
box1 =
    Entity
        [ Drawable
        , Shape
            (Components.BoundingBox2d
                (BoundingBox2d.with
                    { minX = 100
                    , maxX = 120
                    , minY = 50
                    , maxY = 70
                    }
                )
            )
        , Draggable Components.NotDragged
        ]


box2 : Entity
box2 =
    Entity
        [ Drawable
        , Shape
            (Components.BoundingBox2d
                (BoundingBox2d.with
                    { minX = 50
                    , maxX = 70
                    , minY = 250
                    , maxY = 300
                    }
                )
            )
        ]


init : ( Model, Cmd msg )
init =
    ( Model Nothing [ box1, box2 ], Cmd.none )


updateEntity : Msg -> Maybe a -> Entity -> Entity
updateEntity msg drag entity =
    case
        drag
    of
        Just dragging ->
            applyDraggable msg dragging entity

        Nothing ->
            entity


updateEntities : Msg -> Model -> Model
updateEntities msg model =
    let
        configuredUpdater =
            updateEntity msg model.drag

        newEntities =
            List.map configuredUpdater model.entities
    in
        { model | entities = newEntities }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msgs.Press pos ->
            ( updateEntities msg { model | drag = Just (Drag pos pos) }, Cmd.none )

        Msgs.Release pos ->
            ( updateEntities msg { model | drag = Nothing }, Cmd.none )

        _ ->
            ( updateEntities msg model, Cmd.none )


renderEntity : Entity -> Html msg
renderEntity entity =
    case
        (getShape entity)
    of
        Just (Shape shape) ->
            case
                shape
            of
                Components.BoundingBox2d box ->
                    let
                        extrema =
                            BoundingBox2d.extrema box
                    in
                        rect [ width (toString (extrema.maxX - extrema.minX)), height (toString (extrema.maxY - extrema.minY)), x (toString extrema.minX), y (toString extrema.minX) ] []

        _ ->
            div [] []


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ id "svg", width "400", height "400", customOnMouseDown ]
            (List.map renderEntity model.entities)
        ]


customOnMouseDown : Html.Attribute Msg
customOnMouseDown =
    let
        decoder =
            Decode.oneOf
                [ Decode.map
                    Msgs.Press
                    (Decode.map2
                        Position
                        (Decode.field "pageX" Decode.int)
                        (Decode.field "pageY" Decode.int)
                    )
                , Decode.succeed (Msgs.Press (Mouse.Position 500 500))
                ]
    in
        Html.Events.on "mousedown" decoder



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves Msgs.Move, Mouse.ups Msgs.Release ]
