module Components
    exposing
        ( Component
            ( Drawable
            , Shape
            , DragStatus
            , DraggableComponent
            , Hoverable
            , Selectable
            , Node
            , Port
            , Node
            , Link
            , Attachment
            , Appearance
            , Brush
            )
        , Hoverable(..)
        , Selectable(..)
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , Port(PortSource, PortSink)
        , Attribute(..)
        , Drag
        )

import Draggable exposing (Draggable)

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import Mouse exposing (Position)


type alias Drag =
    { startPos : Mouse.Position
    , previousPos : Mouse.Position
    , currentPos : Mouse.Position
    }


type alias Position =
    Point2d





type Hoverable
    = Hovered (List Attribute)
    | NotHovered (List Attribute)


type Selectable
    = NotSelected (List Attribute)
    | Pressed ( Mouse.Position, List Attribute )
    | Selected (List Attribute)


type Shape
    = BoundingBox2d BoundingBox2d
    | Circle2d Circle2d
    | LineSegment2d LineSegment2d


type Port
    = PortSource String
    | PortSink String


type Attribute
    = Stroke String
    | StrokeWidth String
    | Fill String
    | Rx String
    | Ry String


type Component
    = Drawable Int
    | Shape Shape
    | DragStatus (Maybe Drag)
    | DraggableComponent Draggable
    | Hoverable Hoverable
    | Selectable Selectable
    | Node
    | Port Port
    | Link String String
    | Attachment String Vector2d
    | Appearance ( List Attribute, List Attribute )
    | Brush Bool
