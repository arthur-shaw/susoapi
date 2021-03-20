# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(approve_interview_as_hq(interview_id = "123"))

})

# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("approve_interview_as_hq_msg", {
        expect_message(approve_interview_as_hq(
                        interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                        verbose = TRUE
                    ))
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("approve_interview_as_hq_logical", {
        suppressMessages(expect_message(approve_interview_as_hq(
                        interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                        verbose = TRUE
                    )))
    })

})
