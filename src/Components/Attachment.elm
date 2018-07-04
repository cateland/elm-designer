module Attachment exposing (Attachment, createAttachment, getAttachmentTarget, setAttachmentTarget, getAttachmentOffset, setAttachmntOffset)

import Vector2d as Vector2d exposing (Vector2d)


type alias Attachment =
    { attachmentId : String
    , offset : Vector2d
    }


createAttachment : String -> Vector2d -> Attachment
createAttachment attachmentId offset =
    Attachment attachmentId offset


getAttachmentTarget : Attachment -> String
getAttachmentTarget attachment =
    attachment.attachmentId


setAttachmentTarget : Attachment -> String -> Attachment
setAttachmentTarget attachment attachmentId =
    { attachment | attachmentId = attachmentId }


getAttachmentOffset : Attachment -> Vector2d
getAttachmentOffset attachment =
    attachment.offset


setAttachmntOffset : Attachment -> Vector2d -> Attachment
setAttachmntOffset attachment offset =
    { attachment | offset = offset }
