module Page.Utils exposing (..)

import Http

errorToString : Http.Error -> String
errorToString e =
  case e of
    Http.BadUrl url ->
        "The URL " ++ url ++ " was invalid"
    Http.Timeout ->
        "Unable to reach the server, try again"
    Http.NetworkError ->
        "Unable to reach the server, check your network connection"
    Http.BadStatus 500 ->
        "The server had a problem, try again later"
    Http.BadStatus 400 ->
        "Verify your information and try again"
    Http.BadStatus _ ->
        "Unknown error"
    Http.BadBody errorMessage ->
        errorMessage