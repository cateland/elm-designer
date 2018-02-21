module HoverableSystem exposing (..)

import Msgs exposing (Msg(Move))
import Entity exposing (Entity, addComponent)
import Components exposing (Component(Shape, HoverableComponent, Appearance))
import HoverableComponent exposing (getHoverable, updateHoverable)
import Hoverable exposing (isHovered, toggleHoverrable, getHoverAppearence)
import Appearance exposing (getAppearance, updateAppearance)
import Shape exposing (..)
import Math exposing (isVectorOver, postionToPoint2d)


applyHoverable : Msgs.Msg -> Entity -> Entity
applyHoverable msg entity =
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
