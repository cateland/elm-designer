module HoverableSystem exposing (..)

import Msgs exposing (Msg(Move))
import Components exposing (Entity, Component(Shape, Hoverable, Appearance), Hoverable, addComponent)
import Hoverable exposing (getHoverable, updateHoverable)
import Appearance exposing (getAppearance, updateAppearance)
import Shape exposing (..)
import Math exposing (isVectorOver, postionToPoint2d)
import Debug


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
                                Just (Appearance initialAppearence) ->
                                    updateHoverable
                                        (Hoverable (Components.Hovered initialAppearence))
                                        (updateAppearance (Appearance hoverAppearence) entity)

                                Nothing ->
                                    updateHoverable
                                        (Hoverable (Components.Hovered []))
                                        (addComponent (Appearance hoverAppearence) entity)

                                _ ->
                                    entity

                        False ->
                            entity

                ( Just (Hoverable (Components.Hovered hoverAppearence)), Just (Shape entityShape) ) ->
                    case
                        isVectorOver (postionToPoint2d position) entityShape
                    of
                        False ->
                            case getAppearance entity of
                                Just (Appearance initialAppearence) ->
                                    updateHoverable
                                        (Hoverable (Components.NotHovered initialAppearence))
                                        (updateAppearance (Appearance hoverAppearence) entity)

                                Nothing ->
                                    updateHoverable
                                        (Hoverable (Components.NotHovered []))
                                        (addComponent (Appearance hoverAppearence) entity)

                                _ ->
                                    entity

                        True ->
                            entity

                _ ->
                    entity

        _ ->
            entity
