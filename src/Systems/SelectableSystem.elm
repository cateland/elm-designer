module SelectableSystem exposing (..)

import Msgs exposing (Msg(Move))
import Entity exposing (Entity, addComponent)
import Components exposing (Component(Shape, Selectable, Appearance), Selectable(..))
import Selectable exposing (getSelectable, updateSelectable)
import Appearance exposing (getAppearance, updateAppearance)
import Shape exposing (..)
import Math exposing (isVectorOver, postionToPoint2d)

-- ok this is super duper unclean
-- check if part of multiple element selected
-- if -> noop
-- problem (biding with MultiSelectDrag :/)

applySelectable : Msgs.Msg -> Entity -> Entity
applySelectable msg entity =
    case
        msg
    of
        Msgs.Press position ->
            case
                ( getSelectable entity, getShape entity, getAppearance entity )
            of
                ( Just (Selectable (Components.NotSelected selectedAppearence)), Just (Shape entityShape), _ ) ->
                    case isVectorOver (postionToPoint2d position) entityShape of
                        True ->
                            updateSelectable (Selectable (Pressed ( position, selectedAppearence ))) entity

                        False ->
                            entity

                ( Just (Selectable (Components.Selected selectedAppearence)), Just (Shape entityShape), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                    case isVectorOver (postionToPoint2d position) entityShape of
                        True ->
                            entity

                        False ->
                            updateSelectable
                                (Selectable (Components.NotSelected selectedAppearence))
                                (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)

                _ ->
                    entity

        Msgs.Release position ->
            case
                ( getSelectable entity, getShape entity )
            of
                ( Just (Selectable (Components.Pressed ( pressPosition, selectedAppearence ))), Just (Shape entityShape) ) ->
                    case getAppearance entity of
                        Just (Appearance ( initialAppearence, overideAppearence )) ->
                            case pressPosition.x == position.x && pressPosition.y == position.y of
                                True ->
                                    updateSelectable
                                        (Selectable (Selected selectedAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity)

                                False ->
                                    updateSelectable
                                        (Selectable (NotSelected selectedAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)

                        Nothing ->
                            entity

                        _ ->
                            entity

                _ ->
                    entity

        _ ->
            case ( getSelectable entity, getAppearance entity ) of
                ( Just (Selectable (Components.Selected selectedAppearence)), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                    updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity

                _ ->
                    entity
