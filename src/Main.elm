module Main exposing (main)

import Html exposing (Html, div, text)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (moves, ups, Position)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (height, id, width, x, y, r, cx, cy)
import Msgs exposing (Msg(..))
import Components
    exposing
        ( Entity(..)
        , Component(..)
        , Draggable(Dragged, NotDragged)
        , Shape(BoundingBox2d, Circle2d)
        , Port(PortSource, PortSink)
        )
import Shape exposing (..)
import DraggableSystem exposing (..)
import PortSystem exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import Math exposing (Drag)
import Dict exposing (Dict)


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
        , Draggable NotDragged
        , Port (PortSource "box2")
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
        , Draggable NotDragged
        , Port (PortSink "box2")
        ]
    )



--{ centerPoint : Point2d, radius : Float }


init : ( Model, Cmd msg )
init =
    ( Model Nothing (Dict.fromList [ box1, box2, circle1, circle2 ]), Cmd.none )


updateEntity : Dict String Entity -> Msg -> Maybe Drag -> String -> Entity -> Entity
updateEntity entities msg drag key components =
    components
        |> applyDraggable msg drag
        |> applyPort entities


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
    case Debug.log "Messages" msg of
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
                    let
                        extrema =
                            BoundingBox2d.extrema box
                    in
                        rect
                            [ width (toString (extrema.maxX - extrema.minX))
                            , height (toString (extrema.maxY - extrema.minY))
                            , x (toString extrema.minX)
                            , y (toString extrema.minY)
                            ]
                            []

                Circle2d circle ->
                    let
                        ( x, y ) =
                            Point2d.coordinates (Circle2d.centerPoint circle)

                        radius =
                            Circle2d.radius circle
                    in
                        Svg.circle [ r (toString radius), cx (toString x), cy (toString y) ] []

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
