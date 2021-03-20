# invalid inputs

test_that("If invalid `id` parameter, then issues error", {
    expect_error(unarchive_assignment(id = "boo"))
})

# valid outputs

test_that("Issues message", {

    vcr::use_cassette("unarchive_assignment_msg", {
        expect_message(unarchive_assignment(id = 1))
    })

})

test_that("Returns logical value if `verbose = TRUE`", {

    vcr::use_cassette("unarchive_assignment_returns", {
        x <- suppressMessages(unarchive_assignment(id = 1, verbose = TRUE))
    })

    expect_type(x, "logical")

})

