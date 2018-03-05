module SelectableSystem exposing (selectableSystem)

import Appearance exposing (getAppearance, updateAppearance)
import Components exposing (Component(Appearance, SelectableComponent, Shape), Selectable(..))
import Entity exposing (Entities, Entity, NewEntities, addComponent)
import Math exposing (isVectorOver, postionToPoint2d)
import Msgs exposing (Msg)
import Selectable exposing (getSelectable, updateSelectable)
import Shape exposing (..)


selectableSystem : Msgs.Msg -> Entities -> String -> (Entity, NewEntities) -> (Entity, NewEntities)
selectableSystem msg entities key (entity, newEntities) =
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
                            (updateSelectable (SelectableComponent (Pressed ( position, selectedAppearence ))) entity, newEntities)

                        False ->
                            (entity, newEntities)

                ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Shape entityShape), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                    case isVectorOver (postionToPoint2d position) entityShape of
                        True ->
                            (entity, newEntities)

                        False ->
                            (updateSelectable
                                (SelectableComponent (Components.NotSelected selectedAppearence))
                                (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity), newEntities)

                _ ->
                    (entity, newEntities)

        Msgs.Release position ->
            case
                ( getSelectable entity, getShape entity )
            of
                ( Just (SelectableComponent (Components.Pressed ( pressPosition, selectedAppearence ))), Just (Shape entityShape) ) ->
                    case getAppearance entity of
                        Just (Appearance ( initialAppearence, overideAppearence )) ->
                            case pressPosition.x == position.x && pressPosition.y == position.y of
                                True ->
                                    (updateSelectable
                                        (SelectableComponent (Selected selectedAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity), newEntities)

                                False ->
                                    (updateSelectable
                                        (SelectableComponent (NotSelected selectedAppearence))
                                        (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity), newEntities)

                        Nothing ->
                            (entity, newEntities)

                        _ ->
                            (entity, newEntities)

                _ ->
                    (entity, newEntities)

        _ ->
            case ( getSelectable entity, getAppearance entity ) of
                ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                    (updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity, newEntities)

                _ ->
                    (entity, newEntities)
