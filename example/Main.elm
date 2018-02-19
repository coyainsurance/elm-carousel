module Main exposing (..)

{-| This is an example using the Carousel
-}

import Html.Styled as Html exposing (Html)
import Carousel exposing (Carousel, CarouselMsg(..), EventMsg(..))


type alias Elements =
    Carousel Msg


main : Program Never Elements Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Elements, Cmd Msg )
init =
    Carousel.fromList (List.repeat 5 (Html.text "this is an example of Html")) ! []


type Msg
    = CarrouselMsgWrapper CarouselMsg


update : Msg -> Elements -> ( Elements, Cmd Msg )
update msg elements =
    case msg of
        CarrouselMsgWrapper carouselMsg ->
            Carousel.sendMsg carouselMsg elements ! []



-- VIEW


view : Elements -> Html Msg
view elements =
    Carousel.view elements CarrouselMsgWrapper
