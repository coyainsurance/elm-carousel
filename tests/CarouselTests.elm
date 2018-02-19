module CarouselTests exposing (..)

{-| Test for the Carousel component
-}

import Carousel exposing (Carousel, CarouselMsg(..), EventMsg(..))
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
        , isActiveTest
        ]


fromListTest : Test
fromListTest =
    describe "fromList"
        [ test "current element is expected" <|
            \_ ->
                Carousel.fromList (List.repeat 3 (Html.text "test"))
                    |> .seats
                    |> ZipList.current
                    |> Expect.equal (Just ( 0, (Html.text "test") ))
        , test "current element is expected after next" <|
            \_ ->
                Carousel.fromList (List.repeat 3 (Html.text "test"))
                    |> .seats
                    |> ZipList.forward
                    |> ZipList.current
                    |> Expect.equal (Just ( 1, (Html.text "test") ))
        , test "current element is expected after previous" <|
            \_ ->
                Carousel.fromList (List.repeat 3 (Html.text "test"))
                    |> .seats
                    |> ZipList.backward
                    |> ZipList.current
                    |> Expect.equal (Just ( 0, (Html.text "test") ))
        ]


isActiveTest : Test
isActiveTest =
    describe "isActive"
        [ test
            ("returns True when the index of the element"
                ++ "is equal than the current one"
            )
          <|
            \_ ->
                (Just ( 0, (Html.text "test") ))
                    |> Carousel.isActive 0
                    |> Expect.equal True
        , test
            ("returns False when the index of the element"
                ++ "is different than the current one"
            )
          <|
            \_ ->
                (Just ( 1, (Html.text "test") ))
                    |> Carousel.isActive 0
                    |> Expect.equal False
        ]
