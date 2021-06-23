# invalid inputs

# assignment ID

test_that("If invalid `id` parameter, then issues error", {
    expect_error(close_assignment(id = "boo"))
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {

    expect_error(
        close_assignment(
            id = 1,
            workspace = "invalid workspace name"
        )
    )

})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {

    expect_error(
        close_assignment(
            id = 1,
            workspace = "fake"
        )
    )

})

# valid outputs

test_that("Issues message", {

    vcr::use_cassette("close_assignment_msg", {
        expect_message(close_assignment(id = 1))
    })

})

test_that("Returns logical value if `verbose = TRUE`", {

    vcr::use_cassette("close_assignment_returns", {
        x <- suppressMessages(close_assignment(id = 1, verbose = TRUE))
    })

    expect_type(x, "logical")

})

