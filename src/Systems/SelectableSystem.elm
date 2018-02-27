module SelectableSystem exposing (selectableSystem)

import Appearance exposing (getAppearance, updateAppearance)
import Components exposing (Component(Appearance, SelectableComponent, Shape), Selectable(..))
import Entity exposing (Entities, Entity, addComponent)
import Math exposing (isVectorOver, postionToPoint2d)
import Msgs exposing (Msg)
import Selectable exposing (getSelectable, updateSelectable)
import Shape exposing (..)


selectableSystem : Msgs.Msg -> Entities -> String -> Entity -> Entity
selectableSystem msg entities key entity =
    case
        msg
    of
        Msgs.Press position ->
            case
                ( getSelectable entity, getShape entity, getAppearance entity )
            of
                ( Just (SelectableComponent (Components.NotSelected selectedAppearence)), Just (Shape entityShape), _ ) ->
                    case isVectorOver (postionToPoint2d position) entityShape of
                        True ->
                            updateSelectable (SelectableComponent (Pressed ( position, selectedAppearence ))) entity

                        False ->
                            entity

                ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Shape entityShape), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                    case isVectorOver (postionToPoint2d position) entityShape of
                        True ->
                            entity

                        False ->
                            updateSelectable
                                (SelectableComponent (Components.NotSelected selectedAppearence))
                                (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)

                _ ->
                    entity

        Msgs.Release position ->
            case
                ( getSelectable entity, getShape entity )
            of
                ( Just (SelectableComponent (Components.Pressed ( pressPosition, selectedAppearence ))), Just (Shape entityShape) ) ->
                    case getAppearance entity of
                        Just (Appearance ( initialAppearence, overideAppearence )) ->
                            case pressPosition.x == position.x && pressPosition.y == position.y of
                                True ->
                                    updateSelectable
                                        (SelectableComponent (Selected selectedAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity)

                                False ->
                                    updateSelectable
                                        (SelectableComponent (NotSelected selectedAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)

                        Nothing ->
                            entity

                        _ ->
                            entity

                _ ->
                    entity

        _ ->
            case ( getSelectable entity, getAppearance entity ) of
                ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                    updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity

                _ ->
                    entity
