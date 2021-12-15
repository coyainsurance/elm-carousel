module Main exposing (..)

{-| This is an example using the Carousel
-}

import Css exposing (..)
import Browser
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (onClick)
import Carousel exposing (Carousel, CarouselMsg(..), Movement(..))


-- CONSTANTS


slides : List (Html Msg)
slides =
    List.repeat 5 (Html.text "this is an example of Html")



-- MODEL


type alias Model =
    { carousel : Carousel
    }


init : () -> ( Model, Cmd Msg )
init _ =
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
    let
        leftArrowColor =
            if 0 == Carousel.currentElement model.carousel then
                (rgba 0 0 0 0.3)
            else
                (rgb 0 0 0)

        rightArrowColor =
            if
                (List.length slides - 1)
                    == Carousel.currentElement model.carousel
            then
                (rgba 0 0 0 0.3)
            else
                (rgb 0 0 0)
    in
        Html.div []
            [ Html.a
                [ css
                    [ cursor pointer
                    , display inlineBlock
                    , position absolute
                    , left zero
                    , top zero
                    , height (78 |> px)
                    , paddingTop (60 |> px)
                    ]
                , onClick (CarouselEvent (MovementMsg Previous))
                ]
                [ Html.span
                    [ css
                        [ width zero
                        , height zero
                        , borderTop3 (60 |> px) solid (rgba 0 0 0 0)
                        , borderBottom3 (60 |> px) solid (rgba 0 0 0 0)
                        , borderRight3 (60 |> px) solid leftArrowColor
                        ]
                    ]
                    []
                ]
            , Carousel.view slides CarouselEvent model.carousel
            , Html.a
                [ css
                    [ cursor pointer
                    , display inlineBlock
                    , position absolute
                    , right zero
                    , top zero
                    , height (78 |> px)
                    , paddingTop (60 |> px)
                    ]
                , onClick (CarouselEvent (MovementMsg Next))
                ]
                [ Html.span
                    [ css
                        [ width zero
                        , height zero
                        , borderTop3 (60 |> px) solid (rgba 0 0 0 0)
                        , borderBottom3 (60 |> px) solid (rgba 0 0 0 0)
                        , borderLeft3 (60 |> px) solid rightArrowColor
                        ]
                    ]
                    []
                ]
            ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
