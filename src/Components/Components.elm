module Components
    exposing
        ( Component
            ( Appearance
            , Attachment
            , Brush
            , DragStatus
            , DraggableComponent
            , Drawable
            , HoverableComponent
            , Link
            , Node
            , Port
            , SelectableComponent
            , Shape
            )
        , Drag
        , Port(PortSink, PortSource)
        , Selectable(..)
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        )

import Attribute exposing (Attribute)
import Draggable exposing (Draggable)
import Hoverable exposing (Hoverable)
import Mouse exposing (Position)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


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
    | SelectableComponent Selectable
    | Node
    | Port Port
    | Link String String
    | Attachment String Vector2d
    | Appearance ( List Attribute, List Attribute )
    | Brush Bool
