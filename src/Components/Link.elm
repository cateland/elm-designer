module Link exposing (Link, createLink, getSource, getTarget)


type alias Link =
    { source : String, target : String }


createLink : String -> String -> Link
createLink source target =
    Link source target


getSource : Link -> String
getSource link =
    link.source


getTarget : Link -> String
getTarget link =
    link.target
