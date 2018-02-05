module Components
    exposing
        ( Entity(Entity)
        , Component(Drawable, Shape, Draggable, Node, Port, Node)
        , Draggable(Dragged, NotDragged)
        , Shape(BoundingBox2d, Circle2d)
        , Port(PortSource, PortSink)
        , addComponent
        )

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)


addComponent : Component -> Entity -> Entity
addComponent component (Entity components) =
    Entity (component :: components)


type alias Position =
    Point2d


type Draggable
    = Dragged
    | NotDragged


type Shape
    = BoundingBox2d BoundingBox2d
    | Circle2d Circle2d


type Port
    = PortSource String
    | PortSink String


type Component
    = Drawable
    | Shape Shape
    | Draggable Draggable
    | Node
    | Port Port
    | Link


type Entity
    = Entity (List Component)
