module Main exposing (..)

{-| This is an example using the Carousel
-}

import Html.Styled as Html exposing (Html)
import Carousel exposing (Carousel, CarouselMsg(..))


-- CONSTANTS


slides : List (Html Msg)
slides =
    List.repeat 5 (Html.text "this is an example of Html")



-- MODEL


type alias Model =
    { carousel : Carousel
    }


init : ( Model, Cmd Msg )
init =
    ( { carousel = Carousel.fromList slides }, Cmd.none )



-- UPDATE


type Msg
    = CarouselEvent CarouselMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CarouselEvent carouselMsg ->
            let
                carousel =
                    Carousel.sendMsg carouselMsg model.carousel
            in
                ( { model | carousel = carousel }
                , Cmd.none
                )



-- VIEW


view : Model -> Html Msg
view model =
    Carousel.view slides CarouselEvent model.carousel



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
