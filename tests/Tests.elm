module Tests exposing (all)

import ElmTest.Extra exposing (Test, describe)
import CarouselTests


all : Test
all =
    describe "Tests"
        [ CarouselTests.all
        ]
