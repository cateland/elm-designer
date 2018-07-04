module Components
    exposing
        ( Component
            ( Appearance
            , AttachmentComponent
            , Brush
            , DragStatus
            , DraggableComponent
            , Drawable
            , HoverableComponent
            , Link
            , Node
            , Port
            , SelectableComponent
            , ShapeComponent
            )
        , Drag
        , Port(PortSink, PortSource)
        , Selectable(..)
        )

import Attribute exposing (Attribute)
import Draggable exposing (Draggable)
import Hoverable exposing (Hoverable)
import Attachment exposing (Attachment)
import Mouse exposing (Position)
import Shape exposing (Shape)
import OpenSolid.Point2d as Point2d exposing (Point2d)


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

type Port
    = PortSource String
    | PortSink String


type Component
    = Drawable Int
    | ShapeComponent Shape
    | DragStatus (Maybe Drag)
    | DraggableComponent Draggable
    | HoverableComponent Hoverable
    | SelectableComponent Selectable
    | Node
    | Port Port
    | Link String String
    | AttachmentComponent Attachment
    | Appearance ( List Attribute, List Attribute )
    | Brush Bool
