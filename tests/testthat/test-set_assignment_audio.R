# invalid inputs

# assignment ID
test_that("If invalid `id` parameter, then issues error", {
    expect_error(set_assignment_audio(id = "boo", enable = TRUE))
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {

    expect_error(
        set_assignment_audio(
            id = 1, 
            enable = TRUE, 
            verbose = TRUE,
            workspace = "invalid workspace name"
        )
    )

})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspacee", {

    expect_error(
        set_assignment_audio(
            id = 1, 
            enable = TRUE, 
            verbose = TRUE,
            workspace = "fake"
        )
    )

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

