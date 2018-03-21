module Model exposing (..)

import Attribute exposing (fill, rx, ry, stroke, strokeWidth)
import Components exposing (Component(..), Port(PortSink, PortSource))
import Dict exposing (Dict)
import Draggable exposing (createNotDragged)
import Entity exposing (Entities, Entity, addEntity, createEntity)
import Hoverable exposing (createNotHovered)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import Shape exposing (Shape(..), createBoundingBox)


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


entities : Entities
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
