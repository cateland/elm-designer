module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (moves, ups, Position)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as Attributes exposing (height, id, width, x, y, r, cx, cy, x1, x2, y1, y2, stroke)
import OpenSolid.Svg as Svg
import Msgs exposing (Msg(..))
import Entity exposing (Entities, Entity, addEntity, createEntity)
import Components
    exposing
        ( Component(..)
        , Draggable
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , Port(PortSource, PortSink)
        , createDragged
        , toggleDraggable
        )
import Shape exposing (..)
import Drawable exposing (..)
import Appearance exposing (..)
import DragSystem exposing (..)
import DraggableSystem exposing (..)
import MultiSelectDragSystem exposing (..)
import HoverableSystem exposing (..)
import SelectableSystem exposing (..)
import BrushSelectSystem exposing (..)
import PortSystem exposing (..)
import AttachmentSystem exposing (..)
import LinkSystem exposing (..)
import BrushSystem exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import Dict exposing (Dict)
import Render exposing (generateEntitySvgAttributes)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { entities : Entities
    }


control : Entity
control =
    createEntity [ Components.DragStatus Nothing ]


box1 : Entity
box1 =
    createEntity
        [ Drawable 70
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
        , Draggable createDragged
        ]


box2 : Entity
box2 =
    createEntity
        [ Drawable 70
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
        , Draggable createDragged
        , Appearance
            ( [ Components.Stroke "#C5C5C5"
              , Components.StrokeWidth "2"
              , Components.Fill "#F6F6F6"
              , Components.Rx "4"
              , Components.Ry "4"
              ]
            , []
            )
        , Selectable
            (Components.NotSelected
                [ Components.Stroke "#67BBFF" ]
            )
        , Hoverable
            (Components.NotHovered
                [ Components.Fill "white" ]
            )
        , Node
        ]


circleComponent : Entity
circleComponent =
    createEntity
        [ Drawable 70
        , Shape
            (Circle2d
                (Circle2d.with
                    { centerPoint = Point2d.fromCoordinates ( 100, 150 )
                    , radius = 50
                    }
                )
            )
        , Draggable createDragged
        , Appearance
            ( [ Components.Stroke "#C5C5C5"
              , Components.StrokeWidth "2"
              , Components.Fill "#F6F6F6"
              ]
            , []
            )
        , Selectable
            (Components.NotSelected
                [ Components.Stroke "red" ]
            )
        , Hoverable
            (Components.NotHovered
                [ Components.Fill "white" ]
            )
        , Node
        ]


circle1 : Entity
circle1 =
    createEntity
        [ Drawable 80
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
            ( [ Components.Stroke "#1563A5"
              , Components.StrokeWidth "2"
              , Components.Fill "white"
              ]
            , []
            )
        ]


circle2 : Entity
circle2 =
    createEntity
        [ Drawable 80
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
            ( [ Components.Stroke "#1563A5"
              , Components.StrokeWidth "2"
              , Components.Fill "white"
              ]
            , []
            )
        ]


link1 : Entity
link1 =
    createEntity
        [ Drawable 90
        , Link "circle1" "circle2"
        , Appearance ( [ Components.Stroke "#1563A5", Components.StrokeWidth "5" ], [] )
        ]


brush : Entity
brush =
    createEntity
        [ Drawable 100
        , Brush True
        , Appearance
            ( [ Components.Stroke "#1563A5"
              , Components.StrokeWidth "2"
              , Components.Fill "rgba(21, 99, 165, 0.1)"
              ]
            , []
            )
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
                |> addEntity "brush" brush
    in
        ( Model entities, Cmd.none )


updateEntity : Entities -> Msg -> String -> Entity -> Entity
updateEntity entities msg key components =
    components
        |> applyDrag msg
        |> applyDraggable entities
        |> applyHoverable msg
        |> applySelectable msg
        |> applyBrushSelect entities
        |> applyPort entities
        |> applyAttachement entities
        |> applyLink entities
        |> applyBrush entities


updateEntities : Msg -> Model -> Model
updateEntities msg model =
    let
        configuredUpdater =
            updateEntity model.entities msg

        -- foldl here so we have acces to updated components during the update loop !!!!!!
        newEntities =
            Dict.map configuredUpdater (applyMultiSelectDrag model.entities)
    in
        { model | entities = newEntities }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateEntities msg model, Cmd.none )


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


filterDrawable : ( String, Entity ) -> Bool
filterDrawable ( key, entity ) =
    case getDrawable entity of
        Just (Drawable _) ->
            True

        _ ->
            False


sortDrawable : ( String, Entity ) -> ( String, Entity ) -> Order
sortDrawable ( _, entity1 ) ( _, entity2 ) =
    case ( getDrawable entity1, getDrawable entity2 ) of
        ( Just (Drawable order1), Just (Drawable order2) ) ->
            compare order1 order2

        _ ->
            LT


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ id "svg", width "700", height "700", customOnMouseDown, customOnWheel ]
            (List.map renderEntity (List.sortWith sortDrawable (List.filter filterDrawable (Dict.toList model.entities))))
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
