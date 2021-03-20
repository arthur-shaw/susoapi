# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(comment_question(interview_id = "123"))

})

# TODO: variable_name - add check

# TODO: row_number - add check


# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("comment_question_msg", {
        expect_message(comment_question(
                        interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                        variable_name = "NOM_PRENOMS",
                        comment = "testing",
                        verbose = TRUE
                    ))
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("comment_question_logical", {
        suppressMessages(expect_message(comment_question(
                        interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                        variable_name = "NOM_PRENOMS",
                        comment = "testing",
                        verbose = TRUE
                    )))
    })

})
