module Main exposing (Model, main, updateEntities, updateEntity)

import Attribute exposing (fill, rx, ry, stroke, strokeWidth)
import Dict exposing (Dict)
import Draggable exposing (Draggable, createNotDragged)
import Port exposing (Port, createPortSource, createPortSink)
import Link exposing (Link)
import Appearence exposing (Appearence, createAppearence)
import AttachmentSystem exposing (attachementSystem)
import DraggableSystem exposing (draggableSystem)
import PortSystem exposing (portSystem)
import LinkSystem exposing (linkSystem)
import Entity
    exposing
        ( Entities
        , Entity
        , Component
        , NewEntities
        , addEntity
        , createEmptyNewEntities
        , getNewEntitiesValues
        , getSeed
        , isNewEntitiesEmpty
        , (<>)
        )
import Hoverable exposing (createNotHovered)
import Html exposing (Html, div, text)
import Html.Events
import Json.Decode as Decode
import Mouse exposing (Position, moves, ups)
import Msgs exposing (Msg(..))
import BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import Circle2d as Circle2d exposing (Circle2d)
import Point2d as Point2d exposing (Point2d)
import Geometry.Svg as Svg
import Random.Pcg exposing (Seed, initialSeed, step)
import Render exposing (generateEntitySvgAttributes)
import Shape exposing (Shape(..), createBoundingBox, createCircle2d)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes as Attributes exposing (height, id, width)


shape : Shape -> Component
shape providedShape entity =
    { entity | shape = Just (providedShape) }


drawable : Int -> Component
drawable depth entity =
    { entity | drawable = Just (depth) }


drag : Draggable -> Component
drag drag entity =
    { entity | drag = Just (drag) }


portComponent : Port -> Component
portComponent portComponent entity =
    { entity | portComponent = Just (portComponent) }


linkComponent : Link -> Component
linkComponent linkComponent entity =
    { entity | linkComponent = Just (linkComponent) }


appearence : Appearence -> Component
appearence appearence entity =
    { entity | appearence = Just (appearence) }


entity : Entity
entity =
    { shape = Nothing
    , attachment = Nothing
    , drawable = Nothing
    , drag = Nothing
    , portComponent = Nothing
    , linkComponent = Nothing
    , appearence = Nothing
    }


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { entities : Entities
    , currentSeed : Seed
    }


box1 : Entity
box1 =
    entity
        <> [ shape
                (createBoundingBox
                    (BoundingBox2d.fromExtrema
                        { minX = 50
                        , maxX = 70
                        , minY = 50
                        , maxY = 70
                        }
                    )
                )
           , drawable 10
           , drag createNotDragged
           ]


box2 : Entity
box2 =
    entity
        <> [ shape
                (createBoundingBox
                    (BoundingBox2d.fromExtrema
                        { minX = 200
                        , maxX = 400
                        , minY = 150
                        , maxY = 250
                        }
                    )
                )
           , drawable 10
           , drag createNotDragged
           , appearence
                (createAppearence
                    [ stroke "#C5C5C5"
                    , strokeWidth "2"
                    , fill "#F6F6F6"
                    , rx "4"
                    , ry "4"
                    ]
                    []
                )
           ]



-- box2 : Entity
-- box2 =
--     createEntity
--         [ SelectableComponent
--             (Components.NotSelected
--                 [ stroke "#67BBFF" ]
--             )
--         , HoverableComponent
--             (createNotHovered
--                 [ fill "white" ]
--             )
--         , Node
--         ]


circleComponent : Entity
circleComponent =
    entity
        <> [ shape
                (createCircle2d
                    (Circle2d.withRadius 50
                        (Point2d.fromCoordinates
                            ( 100, 150 )
                        )
                    )
                )
           , drawable 10
           , drag createNotDragged
           , appearence
                (createAppearence
                    [ stroke "#C5C5C5"
                    , strokeWidth "2"
                    , fill "#F6F6F6"
                    ]
                    []
                )
           ]



-- circleComponent : Entity
-- circleComponent =
--     createEntity
--         [ SelectableComponent
--             (Components.NotSelected
--                 [ stroke "red" ]
--             )
--         , HoverableComponent
--             (createNotHovered
--                 [ fill "white" ]
--             )
--         , Node
--         ]


circle1 : Entity
circle1 =
    entity
        <> [ shape
                (createCircle2d
                    (Circle2d.withRadius 10
                        (Point2d.fromCoordinates
                            ( 70, 120 )
                        )
                    )
                )
           , drawable 10
           , drag createNotDragged
           , portComponent (createPortSource "circle0")
           , appearence
                (createAppearence
                    [ stroke "#1563A5"
                    , strokeWidth "2"
                    , fill "white"
                    ]
                    []
                )
           ]


circle2 : Entity
circle2 =
    entity
        <> [ shape
                (createCircle2d
                    (Circle2d.withRadius 10
                        (Point2d.fromCoordinates
                            ( 70, 120 )
                        )
                    )
                )
           , drawable 10
           , drag createNotDragged
           , portComponent (createPortSink "box2")
           , appearence
                (createAppearence
                    [ stroke "#1563A5"
                    , strokeWidth "2"
                    , fill "white"
                    ]
                    []
                )
           ]


link1 : Entity
link1 =
    entity
        <> [ linkComponent (Link "circle1" "circle2")
           , drawable 90
           , appearence
                (createAppearence
                    [ stroke "#1563A5"
                    , strokeWidth "5"
                    ]
                    []
                )
           ]



-- brush : Entity
-- brush =
--     createEntity
--         [ Drawable 100
--         , Brush True
--         , Appearance
--             ( [ stroke "#1563A5"
--               , strokeWidth "2"
--               , fill "rgba(21, 99, 165, 0.1)"
--               ]
--             , []
--             )
--         ]


init : ( Model, Cmd msg )
init =
    let
        seed =
            initialSeed 0

        entities =
            Dict.empty
                -- |> addEntity "control" control
                |> addEntity "box1" box1
                |> addEntity "box2" box2
                |> addEntity "circle0" circleComponent
                |> addEntity "circle1" circle1
                |> addEntity "circle2" circle2
                |> addEntity "link1" link1

        -- |> addEntity "brush" brush
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
                |> draggableSystem msg entities key
                |> attachementSystem msg entities key
                |> portSystem msg entities key
                |> linkSystem msg entities key
    in
        case isNewEntitiesEmpty newEntities of
            True ->
                ( seed, Dict.insert key updatedEntity entities )

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
createSvgAttribtues entity =
    Maybe.withDefault []
        (entity
            |> .appearence
            |> Maybe.map generateEntitySvgAttributes
        )


renderEntity : ( String, Entity ) -> Html msg
renderEntity ( key, entity ) =
    case
        entity.shape
    of
        Just shape ->
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
    case entity.drawable of
        Just _ ->
            True

        Nothing ->
            False


sortDrawable : ( String, Entity ) -> ( String, Entity ) -> Order
sortDrawable ( _, entity1 ) ( _, entity2 ) =
    case ( entity1.drawable, entity2.drawable ) of
        ( Just order1, Just order2 ) ->
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
