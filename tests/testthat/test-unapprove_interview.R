# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(unapprove_interview(interview_id = "123"))

})

# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("unapprove_interview_msg", {
        expect_message(
            unapprove_interview(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f"
            )
        )
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("unapprove_interview_logical", {
        x <- suppressMessages(
            expect_message(
                unapprove_interview(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                    verbose = TRUE
                )
            )
        )
    })

    expect_type(x, "logical")

})

