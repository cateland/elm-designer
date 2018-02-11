module Components
    exposing
        ( Entity(Entity)
        , Component(Drawable, Shape, Draggable, Hoverable, Node, Port, Node, Link, Attachment, Appearance)
        , Draggable(Dragged, NotDragged)
        , Hoverable(..)
        , Shape(BoundingBox2d, Circle2d, LineSegment2d)
        , Port(PortSource, PortSink)
        , Attribute(..)
        , addComponent
        )

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)


addComponent : Component -> Entity -> Entity
addComponent component (Entity components) =
    Entity (component :: components)


type alias Position =
    Point2d


type Draggable
    = Dragged
    | NotDragged


type Hoverable
    = Hovered (List Attribute)
    | NotHovered (List Attribute)


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


type Component
    = Drawable
    | Shape Shape
    | Draggable Draggable
    | Hoverable Hoverable
    | Node
    | Port Port
    | Link String String
    | Attachment String Vector2d
    | Appearance (List Attribute)


type Entity
    = Entity (List Component)
