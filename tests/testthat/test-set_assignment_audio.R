# invalid inputs

test_that("If invalid `id` parameter, then issues error", {
    expect_error(set_assignment_audio(id = "boo", enable = TRUE))
})

# valid outputs

test_that("Issues message", {

    vcr::use_cassette("set_assignment_audio_msg", {
        expect_message(set_assignment_audio(id = 1, enable = TRUE, verbose = TRUE))
    })

    

})

test_that("Returns logical value if `verbose = TRUE`", {

    vcr::use_cassette("set_assignment_audio_returns", {
        x <- suppressMessages(set_assignment_audio(id = 1, enable = TRUE, verbose = TRUE))
    })

    expect_type(x, "logical")

})

