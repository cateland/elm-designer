module Components
    exposing
        ( Component
            ( Drawable
            , Shape
            , DragStatus
            , DraggableComponent
            , HoverableComponent
            , Selectable
            , Node
            , Port
            , Node
            , Link
            , Attachment
            , Appearance
            , Brush
            )
        , Selectable(..)
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , Port(PortSource, PortSink)
        , Drag
        )

import Draggable exposing (Draggable)
import Attribute exposing (Attribute)
import Hoverable exposing (Hoverable)

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





type Component
    = Drawable Int
    | Shape Shape
    | DragStatus (Maybe Drag)
    | DraggableComponent Draggable
    | HoverableComponent Hoverable
    | Selectable Selectable
    | Node
    | Port Port
    | Link String String
    | Attachment String Vector2d
    | Appearance ( List Attribute, List Attribute )
    | Brush Bool
