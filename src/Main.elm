module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (moves, ups, Position)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as Attributes exposing (height, id, width, x, y, r, cx, cy, x1, x2, y1, y2, stroke, strokeWidth)
import OpenSolid.Svg as Svg
import Msgs exposing (Msg(..))
import Components
    exposing
        ( Entities
        , Entity(..)
        , Component(..)
        , Draggable(Dragged, NotDragged)
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , Port(PortSource, PortSink)
        , addEntity
        )
import Shape exposing (..)
import Appearance exposing (..)
import DragSystem exposing (..)
import DraggableSystem exposing (..)
import HoverableSystem exposing (..)
import PortSystem exposing (..)
import AttachmentSystem exposing (..)
import LinkSystem exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Render exposing (generateEntitySvgAttributes)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { drag : Maybe Components.Drag
    , entities : Entities
    }


control : Entity
control =
    Entity [ Components.DragStatus Nothing ]


box1 : Entity
box1 =
    Entity
        [ Drawable
        , Shape
            (BoundingBox2d
                (BoundingBox2d.with
                    { minX = 50
                    , maxX = 70
                    , minY = 50
                    , maxY = 70
                    }
                )
            )
        , Draggable NotDragged
        ]


box2 : Entity
box2 =
    Entity
        [ Drawable
        , Shape
            (BoundingBox2d
                (BoundingBox2d.with
                    { minX = 200
                    , maxX = 400
                    , minY = 150
                    , maxY = 250
                    }
                )
            )
        , Draggable NotDragged
        , Appearance
            [ Components.Stroke "#C5C5C5"
            , Components.StrokeWidth "2"
            , Components.Fill "#F6F6F6"
            , Components.Rx "4"
            , Components.Ry "4"
            ]
        , Hoverable
            (Components.NotHovered
                [ Components.Stroke "#C5C5C5"
                , Components.StrokeWidth "2"
                , Components.Fill "white"
                , Components.Rx "4"
                , Components.Ry "4"
                ]
            )
        , Node
        ]


circleComponent : Entity
circleComponent =
    Entity
        [ Drawable
        , Shape
            (Circle2d
                (Circle2d.with
                    { centerPoint = Point2d.fromCoordinates ( 300, 300 )
                    , radius = 50
                    }
                )
            )
        , Draggable NotDragged
        , Appearance
            [ Components.Stroke "#C5C5C5"
            , Components.StrokeWidth "2"
            , Components.Fill "#F6F6F6"
            ]
        , Hoverable
            (Components.NotHovered
                [ Components.Stroke "#C5C5C5"
                , Components.StrokeWidth "2"
                , Components.Fill "white"
                ]
            )
        , Node
        ]


circle1 : Entity
circle1 =
    Entity
        [ Drawable
        , Shape
            (Circle2d
                (Circle2d.with
                    { centerPoint = Point2d.fromCoordinates ( 70, 120 )
                    , radius = 10
                    }
                )
            )
        , Port (PortSource "circle0")
        , Appearance
            [ Components.Stroke "#1563A5"
            , Components.StrokeWidth "2"
            , Components.Fill "white"
            ]
        ]


circle2 : Entity
circle2 =
    Entity
        [ Drawable
        , Shape
            (Circle2d
                (Circle2d.with
                    { centerPoint = Point2d.fromCoordinates ( 70, 120 )
                    , radius = 10
                    }
                )
            )
        , Port (PortSink "box2")
        , Appearance
            [ Components.Stroke "#1563A5"
            , Components.StrokeWidth "2"
            , Components.Fill "white"
            ]
        ]


link1 : Entity
link1 =
    Entity
        [ Drawable
        , Link "circle1" "circle2"
        , Appearance [ Components.Stroke "#1563A5", Components.StrokeWidth "5" ]
        ]



--{ centerPoint : Point2d, radius : Float }


init : ( Model, Cmd msg )
init =
    let
        entities =
            Dict.empty
                |> addEntity "control" control
                |> addEntity "box1" box1
                |> addEntity "box2" box2
                |> addEntity "circle0" circleComponent
                |> addEntity "circle1" circle1
                |> addEntity "circle2" circle2
                |> addEntity "link1" link1
    in
        ( Model Nothing entities, Cmd.none )


updateEntity : Entities -> Msg -> Maybe Components.Drag -> String -> Entity -> Entity
updateEntity entities msg drag key components =
    components
        |> applyDrag msg
        |> applyDraggable msg drag
        |> applyHoverable msg
        |> applyPort entities
        |> applyAttachement entities
        |> applyLink entities


updateEntities : Msg -> Model -> Model
updateEntities msg model =
    let
        configuredUpdater =
            updateEntity model.entities msg model.drag

        newEntities =
            Dict.map configuredUpdater model.entities
    in
        { model | entities = newEntities }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "messages" msg of
        Press pos ->
            ( updateEntities msg { model | drag = Just (Components.Drag pos pos pos) }, Cmd.none )

        Release pos ->
            ( updateEntities msg { model | drag = Nothing }, Cmd.none )

        Move pos ->
            case
                model.drag
            of
                Just drag ->
                    let
                        newDrag =
                            Just (Components.Drag drag.startPos drag.currentPos pos)
                    in
                        ( updateEntities msg { model | drag = newDrag }, Cmd.none )

                Nothing ->
                    ( updateEntities msg model, Cmd.none )

        _ ->
            ( updateEntities msg { model | drag = Nothing }, Cmd.none )


createSvgAttribtues : Entity -> List (Svg.Attribute msg)
createSvgAttribtues =
    generateEntitySvgAttributes
        << getAppearance


renderEntity : ( String, Entity ) -> Html msg
renderEntity ( key, entity ) =
    case
        (getShape entity)
    of
        Just (Shape shape) ->
            case
                shape
            of
                BoundingBox2d box ->
                    Svg.boundingBox2d
                        (createSvgAttribtues entity)
                        (box)

                Circle2d circle ->
                    Svg.circle2d
                        (createSvgAttribtues entity)
                        (circle)

                LineSegment2d lineSegment ->
                    Svg.lineSegment2d
                        (createSvgAttribtues entity)
                        (lineSegment)

        _ ->
            div [] []


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ id "svg", width "700", height "700", customOnMouseDown, customOnWheel ]
            (List.map renderEntity (Dict.toList model.entities))
        ]


customOnMouseDown : Html.Attribute Msg
customOnMouseDown =
    let
        decoder =
            Decode.oneOf
                [ Decode.map
                    Press
                    (Decode.map2
                        Position
                        (Decode.field "pageX" Decode.int)
                        (Decode.field "pageY" Decode.int)
                    )
                , Decode.succeed (Press (Position 500 500))
                ]
    in
        Html.Events.on "mousedown" decoder


customOnWheel : Html.Attribute Msg
customOnWheel =
    let
        decoder =
            Decode.oneOf
                [ Decode.map Zoom (Decode.field "deltaY" Decode.int) ]
    in
        Html.Events.on "wheel" decoder



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Mouse.moves Move, Mouse.ups Release ]
