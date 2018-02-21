module HoverableSystem exposing (..)

import Appearance exposing (getAppearance, updateAppearance)
import Components exposing (Component(Appearance, Hoverable, Shape), Hoverable)
import Entity exposing (Entity, addComponent)
import Hoverable exposing (getHoverable, updateHoverable)
import Math exposing (isVectorOver, postionToPoint2d)
import Msgs exposing (Msg(Move))
import Shape exposing (..)


applyHoverable : Msgs.Msg -> Entity -> Entity
applyHoverable msg entity =
    case
        msg
    of
        Move position ->
            case
                ( getHoverable entity, getShape entity )
            of
                ( Just (Hoverable (Components.NotHovered hoverAppearence)), Just (Shape entityShape) ) ->
                    case
                        isVectorOver (postionToPoint2d position) entityShape
                    of
                        True ->
                            case getAppearance entity of
                                Just (Appearance ( initialAppearence, overideAppearence )) ->
                                    updateHoverable
                                        (Hoverable (Components.Hovered hoverAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, hoverAppearence )) entity)

                                Nothing ->
                                    updateHoverable
                                        (Hoverable (Components.Hovered hoverAppearence))
                                        (addComponent (Appearance ( [], hoverAppearence )) entity)

                                _ ->
                                    entity

                        False ->
                            case getAppearance entity of
                                Just (Appearance ( initialAppearence, _ )) ->
                                    updateAppearance (Appearance ( initialAppearence, [] )) entity

                                _ ->
                                    entity

                ( Just (Hoverable (Components.Hovered hoverAppearence)), Just (Shape entityShape) ) ->
                    case
                        isVectorOver (postionToPoint2d position) entityShape
                    of
                        False ->
                            case getAppearance entity of
                                Just (Appearance ( initialAppearence, _ )) ->
                                    updateHoverable
                                        (Hoverable (Components.NotHovered hoverAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, [] )) entity)

                                Nothing ->
                                    updateHoverable
                                        (Hoverable (Components.NotHovered hoverAppearence))
                                        (addComponent (Appearance ( [], [] )) entity)

                                _ ->
                                    entity

                        True ->
                            entity

                _ ->
                    entity

        _ ->
            entity
