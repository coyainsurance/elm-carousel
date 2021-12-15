module CarouselTests exposing (..)

{-| Test for the Carousel component
-}

import Carousel exposing (Carousel, CarouselMsg(..), Movement(..), Touch(..))
import Expect exposing (Expectation)
import Html.Events.Extra.Touch as Touch exposing (Event, Keys, Touch)
import Html.Styled as Html exposing (Html)
import Json.Encode exposing (Value, bool, float, int, list, object)
import Test exposing (Test, describe, test)
import ZipList



-- Mocks


touchJson : Value
touchJson =
    object
        [ ( "identifier", int 3 )
        , ( "clientX", float 100.0 )
        , ( "clientY", float 0.0 )
        , ( "pageX", float 200.0 )
        , ( "pageY", float 0.0 )
        , ( "screenX", float 300.0 )
        , ( "screenY", float 0.0 )
        ]


eventMock : Event
eventMock =
    Event
        (Keys False False False)
        [ Touch 3 ( 100.0, 0.0 ) ( 200.0, 0.0 ) ( 300.0, 0.0 ) ]
        [ Touch 3 ( 100.0, 0.0 ) ( 200.0, 0.0 ) ( 300.0, 0.0 ) ]
        [ Touch 3 ( 100.0, 0.0 ) ( 200.0, 0.0 ) ( 300.0, 0.0 ) ]


carouselMock : Carousel
carouselMock =
    Carousel
        0.0
        0.0
        True
        (ZipList.fromList (List.range 0 2))



-- TESTS


all : Test
all =
    describe "Carousel"
        [ constructorTests
        , msgsTests
        , selectElementTests
        , currentElementTests
        ]


constructorTests : Test
constructorTests =
    describe "function"
        [ fromListTests
        ]


fromListTests : Test
fromListTests =
    test "`fromList` creates a `Carousel` from an `List (Html msg)`" <|
        \_ ->
            Carousel.fromList (List.repeat 3 (Html.text "test"))
                |> Expect.all
                    [ Expect.equal 0.0 << .transformX
                    , Expect.equal 0.0 << .startingPointX
                    , Expect.equal True << .isAnimated
                    , Expect.equal (ZipList.fromList (List.range 0 2))
                        << .seatIndexes
                    ]


msgsTests : Test
msgsTests =
    describe "sendMsg"
        [ test "`TouchMsg (Start Event)` updates `startingPointX`" <|
            \_ ->
                Carousel.sendMsg (TouchMsg (Start eventMock)) carouselMock
                    |> Expect.equal 100.0
                    << .startingPointX
        , test "`TouchMsg (Move Event)` updates `transformX`" <|
            \_ ->
                Carousel.sendMsg (TouchMsg (Move eventMock)) carouselMock
                    |> Expect.equal 100.0
                    << .transformX
        , test """
        `TouchMsg (End Event)` select next element, change the startingPointX
        and reset the animation state to true
        """ <|
            \_ ->
                let
                    mock =
                        Carousel
                            100.0
                            100.0
                            False
                            (ZipList.fromList (List.range 0 2))
                in
                Carousel.sendMsg (TouchMsg (End eventMock)) mock
                    |> Expect.all
                        [ Expect.equal 0.0 << .transformX
                        , Expect.equal 100.0 << .startingPointX
                        , Expect.equal True << .isAnimated
                        , Expect.equal
                            (List.range 0 2
                                |> ZipList.fromList
                                |> ZipList.forward
                            )
                            << .seatIndexes
                        ]
        , test "MovementMsg Previous select the previous element" <|
            \_ ->
                Carousel.sendMsg (MovementMsg Previous) carouselMock
                    |> Expect.equal
                        (List.range 0 2
                            |> ZipList.fromList
                            |> ZipList.backward
                        )
                    << .seatIndexes
        , test "MovementMsg Next select the next element" <|
            \_ ->
                Carousel.sendMsg (MovementMsg Next) carouselMock
                    |> Expect.equal
                        (List.range 0 2
                            |> ZipList.fromList
                            |> ZipList.forward
                        )
                    << .seatIndexes
        ]


selectElementTests : Test
selectElementTests =
    describe "selectElement"
        [ test "returns the requested element" <|
            \_ ->
                Carousel.selectElement 2 True carouselMock
                    |> Expect.equal
                        (List.range 0 2
                            |> ZipList.fromList
                            |> ZipList.forward
                            |> ZipList.forward
                        )
                    << .seatIndexes
        , test "don't goes out of range with big values" <|
            \_ ->
                Carousel.selectElement 10 True carouselMock
                    |> Expect.equal
                        (List.range 0 2
                            |> ZipList.fromList
                            |> ZipList.forward
                            |> ZipList.forward
                        )
                    << .seatIndexes
        , test "don't goes out of range with smaller values" <|
            \_ ->
                Carousel.selectElement -1 True carouselMock
                    |> Expect.equal
                        (List.range 0 2
                            |> ZipList.fromList
                        )
                    << .seatIndexes
        , test "disable the animations" <|
            \_ ->
                Carousel.selectElement -1 False carouselMock
                    |> Expect.equal False
                    << .isAnimated
        ]


currentElementTests : Test
currentElementTests =
    describe "currentElement"
        [ test "returns the current element" <|
            \_ ->
                let
                    mock =
                        Carousel.selectElement 2 True carouselMock
                in
                Carousel.currentElement mock
                    |> Expect.equal 2
        , test "returns 0 with empty lists" <|
            \_ ->
                Carousel.currentElement (Carousel.fromList [])
                    |> Expect.equal 0
        ]
