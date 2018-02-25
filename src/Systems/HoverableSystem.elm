module HoverableSystem exposing (hoverableSystem)

import Appearance exposing (getAppearance, updateAppearance)
import Components exposing (Component(Appearance, HoverableComponent, Shape))
import Entity exposing (Entities, Entity, addComponent)
import Hoverable exposing (getHoverAppearence, isHovered, toggleHoverrable)
import HoverableComponent exposing (getHoverable, updateHoverable)
import Math exposing (isVectorOver, postionToPoint2d)
import Msgs exposing (Msg(Move))
import Shape exposing (..)


hoverableSystem : Msgs.Msg -> Entities -> String -> Entity -> Entity
hoverableSystem msg entities key entity =
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
                                isVectorOver (postionToPoint2d position) entityShape
                            of
                                False ->
                                    case getAppearance entity of
                                        Just (Appearance ( initialAppearence, _ )) ->
                                            updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (updateAppearance (Appearance ( initialAppearence, [] )) entity)

                                        Nothing ->
                                            updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (addComponent (Appearance ( [], [] )) entity)

                                        _ ->
                                            entity

                                True ->
                                    entity

                        False ->
                            case
                                isVectorOver (postionToPoint2d position) entityShape
                            of
                                True ->
                                    case getAppearance entity of
                                        Just (Appearance ( initialAppearence, overideAppearence )) ->
                                            updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (updateAppearance (Appearance ( initialAppearence, getHoverAppearence hoverStatus )) entity)

                                        Nothing ->
                                            updateHoverable
                                                (HoverableComponent (toggleHoverrable hoverStatus))
                                                (addComponent (Appearance ( [], getHoverAppearence hoverStatus )) entity)

                                        _ ->
                                            entity

                                False ->
                                    case getAppearance entity of
                                        Just (Appearance ( initialAppearence, _ )) ->
                                            updateAppearance (Appearance ( initialAppearence, [] )) entity

                                        _ ->
                                            entity

                _ ->
                    entity

        _ ->
            entity
