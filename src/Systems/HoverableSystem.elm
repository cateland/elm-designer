module HoverableSystem exposing (hoverableSystem)

import Entity exposing (Entities, Entity, NewEntities)
import Hoverable exposing (getHoverAppearence, isHovered, toggleHoverrable)
import Math exposing (positionToPoint2d)
import Msgs exposing (Msg(Move))
import Shape exposing (Shape, isVectorOver)
import Appearence exposing (setOverrideAppearence, clearOverrideAppearence, getInitialAppearence)


hoverableSystem : Msgs.Msg -> Entities -> String -> ( Entity, NewEntities ) -> ( Entity, NewEntities )
hoverableSystem msg entities key tuple =
    let
        ( entity, newEntities ) =
            tuple
    in
        case
            msg
        of
            Move position ->
                case .hover entity of
                    Just hoverStatus ->
                        case .shape entity of
                            Just entityShape ->
                                case isHovered hoverStatus of
                                    True ->
                                        case
                                            isVectorOver (positionToPoint2d position) entityShape
                                        of
                                            False ->
                                                case .appearence entity of
                                                    Just appearence ->
                                                        ( { entity | hover = Just (toggleHoverrable hoverStatus), appearence = Just (setOverrideAppearence (getHoverAppearence hoverStatus) appearence) }, newEntities )

                                                    Nothing ->
                                                        ( { entity | hover = Just (toggleHoverrable hoverStatus) }, newEntities )

                                            True ->
                                                tuple

                                    False ->
                                        case
                                            isVectorOver (positionToPoint2d position) entityShape
                                        of
                                            True ->
                                                case .appearence entity of
                                                    Just appearence ->
                                                        ( { entity | hover = Just (toggleHoverrable hoverStatus), appearence = Just (setOverrideAppearence (getHoverAppearence hoverStatus) appearence) }, newEntities )

                                                    Nothing ->
                                                        ( { entity | hover = Just (toggleHoverrable hoverStatus) }, newEntities )

                                            False ->
                                                case .appearence entity of
                                                    Just appearence ->
                                                        ( { entity | appearence = Just (clearOverrideAppearence appearence) }, newEntities )

                                                    _ ->
                                                        tuple

                            _ ->
                                tuple

                    _ ->
                        tuple

            _ ->
                tuple
