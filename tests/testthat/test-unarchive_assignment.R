# invalid inputs

# assignment ID

test_that("If invalid `id` parameter, then issues error", {
    expect_error(unarchive_assignment(id = "boo"))
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {

    expect_error(
        unarchive_assignment(
            id = 1, 
            verbose = TRUE,
            workspace = "invalid workspace name"
        )
    )

})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {

    expect_error(
        unarchive_assignment(
            id = 1, 
            verbose = TRUE,
            workspace = "fake"
        )
    )

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

