module HoverableSystem exposing (hoverableSystem)

import Appearance exposing (getAppearance, updateAppearance)
import Components exposing (Component(Appearance, HoverableComponent, Shape))
import Entity exposing (Entities, Entity, NewEntities, addComponent)
import Hoverable exposing (getHoverAppearence, isHovered, toggleHoverrable)
import HoverableComponent exposing (getHoverable, updateHoverable)
import Math exposing (isVectorOver, positionToPoint2d)
import Msgs exposing (Msg(Move))
import Shape exposing (..)


hoverableSystem : Msgs.Msg -> Entities -> String -> (Entity, NewEntities) -> (Entity, NewEntities)
hoverableSystem msg entities key (entity, newEntities) =
    case
        msg
    of
        Move position ->
            case
                ( getHoverable entity, getShape entity )
            of
                ( Just (HoverableComponent hoverStatus), Just (Shape entityShape) ) ->
                    case isHovered hoverStatus of
                        True ->
                            case
                                isVectorOver (positionToPoint2d position) entityShape
                            of
                                False ->
                                    case getAppearance entity of
                                        Just (Appearance ( initialAppearence, _ )) ->
                                            (updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (updateAppearance (Appearance ( initialAppearence, [] )) entity), newEntities)
                                        Nothing ->
                                            (updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (addComponent (Appearance ( [], [] )) entity), newEntities)
                                        _ ->
                                             (entity, newEntities)

                                True ->
                                     (entity, newEntities)

                        False ->
                            case
                                isVectorOver (positionToPoint2d position) entityShape
                            of
                                True ->
                                    case getAppearance entity of
                                        Just (Appearance ( initialAppearence, overideAppearence )) ->
                                            (updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (updateAppearance (Appearance ( initialAppearence, getHoverAppearence hoverStatus )) entity), newEntities)

                                        Nothing ->
                                            (updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (addComponent (Appearance ( [], getHoverAppearence hoverStatus )) entity), newEntities)

                                        _ ->
                                             (entity, newEntities)

                                False ->
                                    case getAppearance entity of
                                        Just (Appearance ( initialAppearence, _ )) ->
                                            (updateAppearance (Appearance ( initialAppearence, [] )) entity, newEntities)

                                        _ ->
                                             (entity, newEntities)

                _ ->
                     (entity, newEntities)

        _ ->
            (entity, newEntities)
