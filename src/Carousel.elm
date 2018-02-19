module Carousel
    exposing
        ( Carousel
        , CarouselMsg(..)
        , SwipeMsg(..)
        , sendMsg
        , fromList
        , Css(..)
        , view
        , unstyledView
        )

{-| This is a library that tries to offer a carousel component in Elm


# Definition

@docs Carousel


# Constructors

@docs fromList


# Updates

@docs CarouselMsg


# View

@docs view


# Advanced


## Ux customizations

@docs Css


## Accurate update

@docs sendMsg, SwipeMsg


## None elm-css views

@docs unstyledView

-}

import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import ZipList exposing (ZipList)
import Touch exposing (Event)
import Tuple


-- CONSTANTS


defaultClientPos : Float
defaultClientPos =
    0.0



-- TYPES


type alias Seat msg =
    Html msg


{-| This record represent the Carousel and is used as to represent the state
of the carousel view. You can check what are the seats, what is the starting
position of the carousel and wath is the actual position of the carousel.

transformX -> Actual amount of pixels that the carousel is translated
startingPointX -> When the actual starts the movement
seats -> Is a Tuple with the index and then the html view

    type alias Model =
        { slides : Carousel Msg
        }

-}
type alias Carousel msg =
    { transformX : Float
    , startingPointX : Float
    , seats : ZipList ( Int, Seat msg )
    }



-- MESSAGES


{-| Instead of use an opaque abstraction like: the `OutMsg` or some internal
`update` function. We expose all the carousel messages, then you can use them to
update the `Carousel Msg` type inside your local model or handle some extra
features by your self in your update.

    type Msg
        = SlidesMsg CarouselMsg

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg elements =
        case msg of
            SlidesMsg carouselMsg ->
                ( { model | slides = Carousel.sendMsg carouselMsg model.slides }
                , Cmd.none
                )

-}
type CarouselMsg
    = None
    | CarouselMovementMsg (SwipeMsg Event)


{-| This are the Touch events that we need in order to offer a good swiping
experience. You can use them to do a more accurate filter on your update function.
-}
type SwipeMsg event
    = Start event
    | Move event
    | End event


handleSwipeMsg : SwipeMsg Event -> Carousel msg -> Carousel msg
handleSwipeMsg eventMsg carousel =
    case eventMsg of
        Start event ->
            { carousel | startingPointX = positionX event }

        Move event ->
            { carousel
                | transformX = (positionX event) - carousel.startingPointX
            }

        End event ->
            let
                endClientPos : Float
                endClientPos =
                    positionX event

                seats : ZipList ( Int, Seat msg )
                seats =
                    case direction carousel.startingPointX endClientPos of
                        Left ->
                            ZipList.backward carousel.seats

                        Right ->
                            ZipList.forward carousel.seats
            in
                { carousel
                    | seats = seats
                    , startingPointX = endClientPos
                    , transformX = defaultClientPos
                }


{-| This function updates a `Carousel Msg` based on a `CarouselMsg`

    { model | slides = Carousel.sendMsg carouselMsg model.slides }

-}
sendMsg : CarouselMsg -> Carousel msg -> Carousel msg
sendMsg carouselMsg carousel =
    case carouselMsg of
        CarouselMovementMsg eventMsg ->
            handleSwipeMsg eventMsg carousel

        None ->
            carousel



-- HELPERS


{-| This function is the **main carousel constructor** creates a `Carousel Msg`
from a `List (Html msg)`

    Carousel.fromList (List.repeat 5 (Html.text "this is an example of Html"))

-}
fromList : List (Html msg) -> Carousel msg
fromList listOfHtml =
    listOfHtml
        |> List.indexedMap (,)
        |> ZipList.fromList
        |> Carousel defaultClientPos defaultClientPos


isActive : Int -> Maybe ( Int, Seat msg ) -> Bool
isActive seatId currentSeat =
    case currentSeat of
        Just ( id, _ ) ->
            seatId == id

        Nothing ->
            False


type Direction
    = Left
    | Right


direction : Float -> Float -> Direction
direction start end =
    if start < end then
        Left
    else
        Right


positionX : Event -> Float
positionX event =
    case (List.foldl (Just >> always) Nothing event.changedTouches) of
        Just touch ->
            Tuple.first touch.clientPos

        Nothing ->
            defaultClientPos



-- VIEWS


{-| List of union types representing all the classes of the carousel, they are
exposed mainly if you need some extra visual customizations of the carousel.
-}
type Css
    = SeatElement
    | SeatsElement
    | Active
    | CarouselElement


eventsList : (CarouselMsg -> msg) -> List (Attribute msg)
eventsList msgConstructor =
    List.map
        Attr.fromUnstyled
        [ Touch.onStart
            (\event -> msgConstructor (CarouselMovementMsg (Start event)))
        , Touch.onMove
            (\event -> msgConstructor (CarouselMovementMsg (Move event)))
        , Touch.onEnd
            (\event -> msgConstructor (CarouselMovementMsg (End event)))
        ]


seatView : ( Int, Seat msg ) -> Maybe ( Int, Seat msg ) -> Html msg
seatView ( id, seat ) currentSeat =
    li
        [ Attr.classList
            [ ( toString SeatElement, True )
            , ( toString Active, isActive id currentSeat )
            ]
        , css
            [ display inlineBlock
            , width (100 |> pct)
            ]
        ]
        [ seat ]


{-| Renders the carousel view

    view : Model -> Html Msg
    view model =
        div [] [ Carousel.view model.slides SlidesMsg ]

-}
view : Carousel msg -> (CarouselMsg -> msg) -> Html msg
view carousel msgConstructor =
    let
        carouselSize =
            toFloat (ZipList.length carousel.seats)

        offsetPct =
            carousel.seats
                |> ZipList.current
                |> Maybe.withDefault ( 0, text "useless" )
                |> Tuple.first
                |> toFloat
                |> (\index -> (-1.0 * (100.0 / carouselSize)) * index)

        transition =
            if carousel.transformX == 0.0 then
                property "transition" "transform 0.8s"
            else
                property "transition" "transform 0.0s"
    in
        div
            [ Attr.class (toString CarouselElement)
            , css
                [ width (100 |> pct)
                , height (100 |> pct)
                , overflow hidden
                ]
            ]
            [ ul
                ([ Attr.class (toString SeatsElement)
                 , css
                    [ listStyle none
                    , margin zero
                    , padding zero
                    , position relative
                    , displayFlex
                    , width
                        (((ZipList.length carousel.seats) * 100)
                            |> toFloat
                            |> pct
                        )
                    , height (100 |> pct)
                    , overflow hidden
                    , transforms
                        [ translateX (offsetPct |> pct)
                        , translateX (carousel.transformX |> px)
                        ]
                    , transition
                    ]
                 ]
                    ++ eventsList msgConstructor
                )
                (carousel.seats
                    |> ZipList.toList
                    |> List.map (flip seatView (ZipList.current carousel.seats))
                )
            ]


{-| This function is here mainly for testing porpoises but developers can find
it useful if they don't want to have support of elm-css.
-}
unstyledView : Carousel msg -> (CarouselMsg -> msg) -> Html.Html msg
unstyledView carousel msgConstructor =
    view carousel msgConstructor
        |> toUnstyled
