module Components
    exposing
        ( Entity(Entity)
        , Component(Drawable, Shape, Draggable)
        , Draggable(Dragged, NotDragged)
        , Shape(BoundingBox2d)
        , addComponent
        )

import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.BoundingBox2d as BoundingBox2d exposing (BoundingBox2d)


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


type Component
    = Drawable
    | Shape Shape
    | Draggable Draggable


type Entity
    = Entity (List Component)
