module Utils exposing (format)


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
