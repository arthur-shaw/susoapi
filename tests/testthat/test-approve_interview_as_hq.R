# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(approve_interview_as_hq(interview_id = "123"))

})

# verbose
test_that("Issues error if `verbose` is invalid type", {

    expect_error(
        approve_interview_as_hq(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            verbose = "TRUE"
        )
    )

})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        approve_interview_as_hq(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            verbose = TRUE,
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        approve_interview_as_hq(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            verbose = TRUE,
            workspace = "fake"
        )
    )
})

# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("approve_interview_as_hq_msg", {
        expect_message(approve_interview_as_hq(interview_id = 
            "7bdf95abab1b4d46b818cdf7546e049f"
        ))
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("approve_interview_as_hq_lgl", {
        x <- suppressMessages(
                approve_interview_as_hq(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                    verbose = TRUE
                )
            )
    })

    expect_type(x, "logical")

})
