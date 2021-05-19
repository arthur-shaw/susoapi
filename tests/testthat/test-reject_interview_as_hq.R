# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(reject_interview_as_hq(interview_id = "123"))

})

# responsible_id, if provided
test_that("Issues error if `responsible_id` is provided and is of invalid form", {

    expect_error(reject_interview_as_hq(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                    responsible_id = "123"
                ))

})

# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("reject_interview_as_hq_msg", {
        expect_message(
            reject_interview_as_hq(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f"
            )
        )
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("reject_interview_as_hq_logical", {
        x <- suppressMessages(
            expect_message(
                reject_interview_as_hq(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                    verbose = TRUE
                )
            )
        )
    })

    expect_type(x, "logical")

})
