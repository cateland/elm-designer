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
        ( Entity(..)
        , Component(..)
        , Draggable(Dragged, NotDragged)
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , Port(PortSource, PortSink)
        )
import Shape exposing (..)
import Appearance exposing (..)
import DraggableSystem exposing (..)
import PortSystem exposing (..)
import AttachmentSystem exposing (..)
import LinkSystem exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import Math exposing (Drag)
import Dict exposing (Dict)
import Render exposing (generateEntitySvgAttributes)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { drag : Maybe Drag
    , entities : Dict String Entity
    }


box1 : ( String, Entity )
box1 =
    ( "box1"
    , Entity
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
    )


box2 : ( String, Entity )
box2 =
    ( "box2"
    , Entity
        [ Drawable
        , Shape
            (BoundingBox2d
                (BoundingBox2d.with
                    { minX = 200
                    , maxX = 300
                    , minY = 250
                    , maxY = 320
                    }
                )
            )
        , Draggable NotDragged
        , Node
        ]
    )


circleComponent : ( String, Entity )
circleComponent =
    ( "circleComponent"
    , Entity
        [ Drawable
        , Shape
            (Circle2d
                (Circle2d.with
                    { centerPoint = Point2d.fromCoordinates ( 300, 300 )
                    , radius = 25
                    }
                )
            )
        , Draggable NotDragged
        , Node
        ]
    )


circle1 : ( String, Entity )
circle1 =
    ( "circle1"
    , Entity
        [ Drawable
        , Shape
            (Circle2d
                (Circle2d.with
                    { centerPoint = Point2d.fromCoordinates ( 70, 120 )
                    , radius = 10
                    }
                )
            )
        , Port (PortSource "circleComponent")
        ]
    )


circle2 : ( String, Entity )
circle2 =
    ( "circle2"
    , Entity
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
        ]
    )


link1 : ( String, Entity )
link1 =
    ( "link1"
    , Entity
        [ Drawable
        , Link "circle1" "circle2"
        , Appearance [ Components.Stroke "red", Components.StrokeWidth "5" ]
        ]
    )



--{ centerPoint : Point2d, radius : Float }


init : ( Model, Cmd msg )
init =
    ( Model Nothing (Dict.fromList [ box1, box2, circleComponent, circle1, circle2, link1 ]), Cmd.none )


updateEntity : Dict String Entity -> Msg -> Maybe Drag -> String -> Entity -> Entity
updateEntity entities msg drag key components =
    components
        |> applyDraggable msg drag
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
    case msg of
        Press pos ->
            ( updateEntities msg { model | drag = Just (Drag pos pos pos) }, Cmd.none )

        Release pos ->
            ( updateEntities msg { model | drag = Nothing }, Cmd.none )

        Move pos ->
            case
                model.drag
            of
                Just drag ->
                    let
                        newDrag =
                            Just (Drag drag.startPos drag.currentPos pos)
                    in
                        ( updateEntities msg { model | drag = newDrag }, Cmd.none )

                Nothing ->
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


view : Model -> Html Msg
view model =
    div
        []
        [ svg
            [ id "svg", width "400", height "400", customOnMouseDown ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves Move, Mouse.ups Release ]
