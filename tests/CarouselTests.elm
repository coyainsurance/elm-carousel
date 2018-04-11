module CarouselTests exposing (..)

{-| Test for the Carousel component
-}

import Carousel exposing (Carousel)
import Html.Styled as Html exposing (Html)
import Expect
import ElmTest.Extra exposing (Test, describe, test)
import ZipList


all : Test
all =
    describe "Carousel"
        [ helpersTests
        ]



-- HELPERS TESTS


helpersTests : Test
helpersTests =
    describe "helper"
        [ fromListTest
        ]


fromListTest : Test
fromListTest =
    describe "fromList"
        [ test "current element is expected" <|
            \_ ->
                Carousel.fromList (List.repeat 3 (Html.text "test"))
                    |> .seatIndexes
                    |> ZipList.current
                    |> Expect.equal (Just 0)
        , test "current element is expected after next" <|
            \_ ->
                Carousel.fromList (List.repeat 3 (Html.text "test"))
                    |> .seatIndexes
                    |> ZipList.forward
                    |> ZipList.current
                    |> Expect.equal (Just 1)
        , test "current element is expected after previous" <|
            \_ ->
                Carousel.fromList (List.repeat 3 (Html.text "test"))
                    |> .seatIndexes
                    |> ZipList.backward
                    |> ZipList.current
                    |> Expect.equal (Just 0)
        ]
