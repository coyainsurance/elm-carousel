module Carousel exposing (..)

{-| This is a library that tries to offer a carousel component in elm
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


type alias Carousel msg =
    { transformX : Float
    , startingPointX : Float
    , seats : ZipList ( Int, Seat msg )
    }



-- CAROUSE MESSAGES


type CarouselMsg
    = None
    | CarouselMovementMsg (EventMsg Event)


type EventMsg event
    = Start event
    | Move event
    | End event


handleEventMsg : EventMsg Event -> Carousel msg -> Carousel msg
handleEventMsg eventMsg carousel =
    case eventMsg of
        Start event ->
            { carousel
                | startingPointX = eventPosition (CarouselMovementMsg eventMsg)
            }

        Move event ->
            let
                actualPos =
                    eventPosition (CarouselMovementMsg eventMsg)
            in
                { carousel | transformX = actualPos - carousel.startingPointX }

        End event ->
            let
                endClientPos : Float
                endClientPos =
                    eventPosition (CarouselMovementMsg eventMsg)

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


sendMsg : CarouselMsg -> Carousel msg -> Carousel msg
sendMsg carouselMsg carousel =
    case carouselMsg of
        CarouselMovementMsg eventMsg ->
            handleEventMsg eventMsg carousel

        None ->
            carousel



-- HELPERS


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


eventPosition : CarouselMsg -> Float
eventPosition msg =
    let
        position : Event -> Float
        position event =
            case (List.foldl (Just >> always) Nothing event.changedTouches) of
                Just touch ->
                    Tuple.first touch.clientPos

                Nothing ->
                    defaultClientPos
    in
        case msg of
            None ->
                defaultClientPos

            CarouselMovementMsg eventMsg ->
                position (extractEvent eventMsg)


extractEvent : EventMsg Event -> Event
extractEvent eventMsg =
    case eventMsg of
        Start event ->
            event

        Move event ->
            event

        End event ->
            event



-- VIEWS


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


unstyledView : Carousel msg -> (CarouselMsg -> msg) -> Html.Html msg
unstyledView carousel msgConstructor =
    view carousel msgConstructor
        |> toUnstyled
