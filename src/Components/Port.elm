module Port exposing (Port, createPortSource, createPortSink, getTarget, isPortSource, isPortSink)


type Port
    = PortSource String
    | PortSink String


createPortSource : String -> Port
createPortSource target =
    PortSource target


createPortSink : String -> Port
createPortSink target =
    PortSink target


isPortSource : Port -> Bool
isPortSource portComponent =
    case portComponent of
        PortSource _ ->
            True

        PortSink _ ->
            False


getTarget : Port -> String
getTarget portComponent =
    case portComponent of
        PortSink target ->
            target

        PortSource target ->
            target


isPortSink : Port -> Bool
isPortSink portComponent =
    case portComponent of
        PortSink _ ->
            True

        PortSource _ ->
            False
