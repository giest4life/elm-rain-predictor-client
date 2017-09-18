module Utils exposing (format, dateToString)

import Date


format : List String -> String -> String
format vals =
    let
        zip w1 w2 =
            case w2 of
                "" ->
                    w1

                w ->
                    w ++ w1
    in
        String.concat << List.map2 zip vals << String.split "%s"


dateToString : Date.Date -> String
dateToString date =
    let
        vals =
            [ toString << Date.dayOfWeek <| date
            , toString << Date.hour <| date
            , toString << Date.minute <| date
            ]

        print =
            Debug.log "time" date
    in
        format vals "%s %s:%s"
