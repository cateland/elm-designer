module SelectableSystem exposing (newSelectableSystem, selectableSystem)

import Appearance exposing (getAppearance, updateAppearance)
import Components exposing (Component(Appearance, SelectableComponent, Shape), Selectable(..))
import Entity exposing (Entities, Entity, NewEntities, addComponent)
import Math exposing (positionToPoint2d)
import Msgs exposing (Msg)
import Selectable exposing (getSelectable, updateSelectable)
import Shape exposing (isVectorOver)
import ShapeComponent exposing (getShape)


selectableSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
selectableSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case
        msg
    of
        Msgs.Press position ->
            case getSelectable entity of
                Just (SelectableComponent (Components.NotSelected selectedAppearence)) ->
                    case
                        ( getShape entity, getAppearance entity )
                    of
                        ( Just (Shape entityShape), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                            if isVectorOver (positionToPoint2d position) entityShape then
                                tuple
                            else
                                ( updateSelectable
                                    (SelectableComponent (Components.NotSelected selectedAppearence))
                                    (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)
                                , newEntities
                                )

                        ( Just (Shape entityShape), _ ) ->
                            if isVectorOver (positionToPoint2d position) entityShape then
                                ( updateSelectable (SelectableComponent (Pressed ( position, selectedAppearence ))) entity, newEntities )
                            else
                                tuple

                        _ ->
                            tuple

                _ ->
                    tuple

        Msgs.Release position ->
            case getSelectable entity of
                Just (SelectableComponent (Components.Pressed ( pressPosition, selectedAppearence ))) ->
                    case
                        getShape entity
                    of
                        Just (Shape entityShape) ->
                            case getAppearance entity of
                                Just (Appearance ( initialAppearence, overideAppearence )) ->
                                    if pressPosition == position then
                                        ( updateSelectable
                                            (SelectableComponent (Selected selectedAppearence))
                                            (updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity)
                                        , newEntities
                                        )
                                    else
                                        ( updateSelectable
                                            (SelectableComponent (NotSelected selectedAppearence))
                                            (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)
                                        , newEntities
                                        )

                                Nothing ->
                                    tuple

                                _ ->
                                    tuple

                        _ ->
                            tuple

                _ ->
                    tuple

        _ ->
            case getSelectable entity of
                Just (SelectableComponent (Components.Selected selectedAppearence)) ->
                    case ( getSelectable entity, getAppearance entity ) of
                        ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                            ( updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity, newEntities )

                        _ ->
                            tuple

                _ ->
                    tuple



newSelectableSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
newSelectableSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
    case
        msg
    of
        Msgs.Press position ->
            case getSelectable entity of
                Just (SelectableComponent (Components.NotSelected selectedAppearence)) ->
                    case
                        ( getShape entity, getAppearance entity )
                    of
                        ( Just (Shape entityShape), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                            if isVectorOver (positionToPoint2d position) entityShape then
                                tuple
                            else
                                ( updateSelectable
                                    (SelectableComponent (Components.NotSelected selectedAppearence))
                                    (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)
                                , newEntities
                                )

                        ( Just (Shape entityShape), _ ) ->
                            if isVectorOver (positionToPoint2d position) entityShape then
                                ( updateSelectable (SelectableComponent (Pressed ( position, selectedAppearence ))) entity, newEntities )
                            else
                                tuple

                        _ ->
                            tuple

                _ ->
                    tuple

        Msgs.Release position ->
            case getSelectable entity of
                Just (SelectableComponent (Components.Pressed ( pressPosition, selectedAppearence ))) ->
                    case
                        getShape entity
                    of
                        Just (Shape entityShape) ->
                            case getAppearance entity of
                                Just (Appearance ( initialAppearence, overideAppearence )) ->
                                    if pressPosition == position then
                                        ( updateSelectable
                                            (SelectableComponent (Selected selectedAppearence))
                                            (updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity)
                                        , newEntities
                                        )
                                    else
                                        ( updateSelectable
                                            (SelectableComponent (NotSelected selectedAppearence))
                                            (updateAppearance (Appearance ( initialAppearence, overideAppearence )) entity)
                                        , newEntities
                                        )

                                Nothing ->
                                    tuple

                                _ ->
                                    tuple

                        _ ->
                            tuple

                _ ->
                    tuple

        _ ->
            case getSelectable entity of
                Just (SelectableComponent (Components.Selected selectedAppearence)) ->
                    case ( getSelectable entity, getAppearance entity ) of
                        ( Just (SelectableComponent (Components.Selected selectedAppearence)), Just (Appearance ( initialAppearence, overideAppearence )) ) ->
                            ( updateAppearance (Appearance ( initialAppearence, List.append overideAppearence selectedAppearence )) entity, newEntities )

                        _ ->
                            tuple

                _ ->
                    tuple
