module Carousel
    exposing
        ( Carousel
        , CarouselMsg(..)
        , Touch(..)
        , Movement(..)
        , sendMsg
        , fromList
        , Css(..)
        , view
        , unstyledView
        , currentElement
        , selectElement
        )

{-| This is a carousel 100% in Elm


# Definition

@docs Carousel


# Constructors

@docs fromList


# Updates

@docs CarouselMsg, Touch, Movement, sendMsg, selectElement


# View

@docs view


# Advanced


## Ux customizations

@docs Css


## Helpers

@docs currentElement


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


{-| This record represent the Carousel and is used as to represent the state
of the carousel view. You can check what are the seats, what is the starting
position of the carousel and wath is the actual position of the carousel.

transformX -> Actual amount of pixels that the carousel is translated
startingPointX -> When the actual carousel starts the movement
seatIndexes -> Is a `ZipList Int` with the indexes of the seats
**(html elements that later will be passed into the view)**

    type alias Model =
        { slides : Carousel
        }

-}
type alias Carousel =
    { transformX : Float
    , startingPointX : Float
    , isAnimated : Bool
    , seatIndexes : ZipList Int
    }



-- MESSAGES


{-| Instead of use an opaque abstraction like: the `OutMsg` or some internal
`update` function. We expose all the carousel messages, then you can use them to
update the `Carousel Msg` type inside your local model or handle some extra
implement extra features by your self in your update function.

    type Msg
        = SlidesMsg CarouselMsg

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg elements =
        case msg of
            SlidesMsg carouselMsg ->
                ( { model | slides = Carousel.sendMsg carouselMsg slides }
                , Cmd.none
                )

-}
type CarouselMsg
    = TouchMsg Touch
    | MovementMsg Movement


{-| This are the Touch events that we need in order to offer a good swiping
experience. You can use them, to do a more accurate pattern matching,
on your main update function.
-}
type Touch
    = Start Event
    | Move Event
    | End Event


handleTouchMsg : Touch -> Carousel -> Carousel
handleTouchMsg eventMsg carousel =
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

                seatIndexes : ZipList Int
                seatIndexes =
                    case direction carousel.startingPointX endClientPos of
                        Left ->
                            ZipList.backward carousel.seatIndexes

                        Right ->
                            ZipList.forward carousel.seatIndexes
            in
                { carousel
                    | seatIndexes = seatIndexes
                    , startingPointX = endClientPos
                    , transformX = defaultClientPos
                    , isAnimated = True
                }


{-| This are movements of the carousel, useful for implement buttons and actions
on desktop environments
-}
type Movement
    = Previous
    | Next


handleMovementMsg : Movement -> Carousel -> Carousel
handleMovementMsg movementMsg carousel =
    case movementMsg of
        Previous ->
            { carousel | seatIndexes = ZipList.backward carousel.seatIndexes }

        Next ->
            { carousel | seatIndexes = ZipList.forward carousel.seatIndexes }


{-| Updates a `Carousel` based on given `CarouselMsg`

    { model | slides = Carousel.sendMsg (TouchMsg (Start event)) model.slides }
    { model | slides = Carousel.sendMsg (TouchMsg (Move event)) model.slides }
    { model | slides = Carousel.sendMsg (TouchMsg (End event)) model.slides }

-}
sendMsg : CarouselMsg -> Carousel -> Carousel
sendMsg carouselMsg carousel =
    case carouselMsg of
        TouchMsg eventMsg ->
            handleTouchMsg eventMsg carousel

        MovementMsg movementMsg ->
            handleMovementMsg movementMsg carousel


{-| Select an specific seat in a carousel, useful to implement pagination you
can enable or disable the animations

    { model | slides = Carousel.selectElement 3 False  model.carousel }
    { model | slides = Carousel.selectElement 3 True  model.carousel }

-}
selectElement : Int -> Bool -> Carousel -> Carousel
selectElement index isAnimated carousel =
    let
        { seatIndexes } =
            carousel

        helperFunc : Int -> ZipList Int -> ZipList Int
        helperFunc index list =
            if index > 0 then
                helperFunc (index - 1) (ZipList.backward list)
            else if index < 0 then
                helperFunc (index + 1) (ZipList.forward list)
            else
                list
    in
        { carousel
            | seatIndexes =
                helperFunc ((currentElement carousel) - index) seatIndexes
            , isAnimated = isAnimated
        }


{-| Display the index of the actual selected seat of a `Carousel` useful
to track from the outside the active seats.

    { model | activeSlide = Carousel.currentElement model.carousel }

-}
currentElement : Carousel -> Int
currentElement { seatIndexes } =
    Maybe.withDefault 0 (ZipList.current seatIndexes)



-- HELPERS


{-| This function is the **main carousel constructor** creates a `Carousel Msg`
from a `List (Html msg)`

    Carousel.fromList (List.repeat 5 (Html.text "this is an example of Html"))

-}
fromList : List (Html msg) -> Carousel
fromList listOfHtml =
    listOfHtml
        |> List.length
        |> flip (-) 1
        |> List.range 0
        |> ZipList.fromList
        |> Carousel defaultClientPos defaultClientPos True


isActive : Int -> Maybe Int -> Bool
isActive seatIndex currentSeatIndex =
    case currentSeatIndex of
        Just id ->
            seatIndex == id

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
exposed mainly if you need some extra visual customization of the carousel.
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
            (\event -> msgConstructor (TouchMsg (Start event)))
        , Touch.onMove
            (\event -> msgConstructor (TouchMsg (Move event)))
        , Touch.onEnd
            (\event -> msgConstructor (TouchMsg (End event)))
        ]


seatView : Maybe Int -> Int -> Html msg -> Html msg
seatView currentSeatIndex index seat =
    li
        [ Attr.classList
            [ ( toString SeatElement, True )
            , ( toString Active, isActive index currentSeatIndex )
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
        div [] [ Carousel.view slides carousel SlidesMsg ]

-}
view : List (Html msg) -> (CarouselMsg -> msg) -> Carousel -> Html msg
view seats msgConstructor carousel =
    let
        carouselSize =
            toFloat (ZipList.length carousel.seatIndexes)

        offsetPct =
            carousel.seatIndexes
                |> ZipList.current
                |> Maybe.withDefault 0
                |> toFloat
                |> (\index -> (-1.0 * (100.0 / carouselSize)) * index)

        transition =
            if carousel.transformX == 0.0 && carousel.isAnimated then
                property "transition" "transform 0.8s"
            else
                property "transition" "transform 0.0s"

        currentSeatIndex =
            ZipList.current carousel.seatIndexes
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
                        (((ZipList.length carousel.seatIndexes) * 100)
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
                (List.indexedMap (seatView currentSeatIndex) seats)
            ]


{-| This function is here mainly for testing porpoises but developers can find
it useful if they don't want to have support of elm-css.
-}
unstyledView :
    List (Html msg)
    -> (CarouselMsg -> msg)
    -> Carousel
    -> Html.Html msg
unstyledView listOfSeats msgConstructor carousel =
    view listOfSeats msgConstructor carousel
        |> toUnstyled
