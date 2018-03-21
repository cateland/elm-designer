module Main exposing (Model, main, updateEntity, updateEntities)

import Appearance exposing (..)
import AttachmentSystem exposing (..)
import Attribute exposing (fill, rx, ry, stroke, strokeWidth)
import BrushSelectSystem exposing (..)
import BrushSystem exposing (..)
import Components
    exposing
        ( Component(..)
        , Port(PortSink, PortSource)
        )
import Dict exposing (Dict)
import DragSystem exposing (dragSystem)
import Draggable exposing (createNotDragged)
import DraggableSystem exposing (draggableSystem)
import Drawable exposing (..)
import Entity
    exposing
        ( Entities
        , Entity
        , NewEntities
        , addEntity
        , createEmptyNewEntities
        , createEntity
        , getNewEntitiesValues
        , getSeed
        , isNewEntitiesEmpty
        )
import Hoverable exposing (createNotHovered)
import HoverableSystem exposing (..)
import Html exposing (Html, div, text)
import Html.Events
import Json.Decode as Decode
import LinkSystem exposing (..)
import Mouse exposing (Position, moves, ups)
import Msgs exposing (Msg(..))
import MultiSelectDragSystem exposing (..)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Svg as Svg
import PortSystem exposing (..)
import Random.Pcg exposing (Seed, initialSeed, step)
import Render exposing (generateEntitySvgAttributes)
import SelectableSystem exposing (..)
import Shape exposing (Shape(..), createBoundingBox)
import ShapeComponent exposing (getShape)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as Attributes exposing (height, id, width)

main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { entities : Entities
    , currentSeed : Seed
    }


control : Entity
control =
    createEntity [ Components.DragStatus Nothing ]


box1 : Entity
box1 =
    createEntity
        [ Drawable 70
        , Shape
            (createBoundingBox
                (BoundingBox2d.with
                    { minX = 50
                    , maxX = 70
                    , minY = 50
                    , maxY = 70
                    }
                )
            )
        , DraggableComponent createNotDragged
        ]


box2 : Entity
box2 =
    createEntity
        [ Drawable 70
        , Shape
            (createBoundingBox
                (BoundingBox2d.with
                    { minX = 200
                    , maxX = 400
                    , minY = 150
                    , maxY = 250
                    }
                )
            )
        , DraggableComponent createNotDragged
        , Appearance
            ( [ stroke "#C5C5C5"
              , strokeWidth "2"
              , fill "#F6F6F6"
              , rx "4"
              , ry "4"
              ]
            , []
            )
        , SelectableComponent
            (Components.NotSelected
                [ stroke "#67BBFF" ]
            )
        , HoverableComponent
            (createNotHovered
                [ fill "white" ]
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
        , DraggableComponent createNotDragged
        , Appearance
            ( [ stroke "#C5C5C5"
              , strokeWidth "2"
              , fill "#F6F6F6"
              ]
            , []
            )
        , SelectableComponent
            (Components.NotSelected
                [ stroke "red" ]
            )
        , HoverableComponent
            (createNotHovered
                [ fill "white" ]
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
            ( [ stroke "#1563A5"
              , strokeWidth "2"
              , fill "white"
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
            ( [ stroke "#1563A5"
              , strokeWidth "2"
              , fill "white"
              ]
            , []
            )
        ]


link1 : Entity
link1 =
    createEntity
        [ Drawable 90
        , Link "circle1" "circle2"
        , Appearance ( [ stroke "#1563A5", strokeWidth "5" ], [] )
        ]


brush : Entity
brush =
    createEntity
        [ Drawable 100
        , Brush True
        , Appearance
            ( [ stroke "#1563A5"
              , strokeWidth "2"
              , fill "rgba(21, 99, 165, 0.1)"
              ]
            , []
            )
        ]



--{ centerPoint : Point2d, radius : Float }


init : ( Model, Cmd msg )
init =
    let
        seed =
            initialSeed 0

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
    ( Model entities seed, Cmd.none )


addNewEntity : ( String, Entity ) -> Entities -> Entities
addNewEntity ( key, entity ) entities =
    Dict.insert key entity entities


addNewEntities : NewEntities -> Entities -> Entities
addNewEntities newEntities entities =
    List.foldl addNewEntity entities (getNewEntitiesValues newEntities)


updateEntity : Msg -> String -> Entity -> ( Seed, Entities ) -> ( Seed, Entities )
updateEntity msg key entity ( seed, entities ) =
    let
        ( updatedEntity, newEntities ) =
            ( entity, createEmptyNewEntities seed )
                |> dragSystem msg entities key
                |> draggableSystem msg entities key
                |> hoverableSystem msg entities key
                |> selectableSystem msg entities key
                |> portSystem msg entities key
                |> attachementSystem msg entities key
                |> linkSystem msg entities key
                |> brushSystem msg entities key
                |> brushSelectSystem msg entities key
                |> multiSelectDragSystem msg entities key
    in
    case isNewEntitiesEmpty newEntities of
        True ->
            ( getSeed newEntities, Dict.insert key updatedEntity entities )

        False ->
            ( getSeed newEntities, addNewEntities newEntities (Dict.insert key updatedEntity entities) )


updateEntities : Msg -> Model -> Model
updateEntities msg model =
    let
        configuredUpdater =
            updateEntity msg

        test =
            Debug.log "count" (Dict.size model.entities)

        -- foldl here so we have acces to updated components during the update loop !!!!!!
        -- foldl doesn't let update the entity :/ guess i have to go on the one iteration per systems :/
        ( seed, newEntities ) =
            Dict.foldl configuredUpdater ( model.currentSeed, model.entities ) model.entities
    in
    { model | entities = newEntities, currentSeed = seed }


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
        getShape entity
    of
        Just (Shape shape) ->
            case
                shape
            of
                BoundingBox2d box ->
                    Svg.boundingBox2d
                        (createSvgAttribtues entity)
                        box

                Circle2d circle ->
                    Svg.circle2d
                        (createSvgAttribtues entity)
                        circle

                LineSegment2d lineSegment ->
                    Svg.lineSegment2d
                        (createSvgAttribtues entity)
                        lineSegment

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
