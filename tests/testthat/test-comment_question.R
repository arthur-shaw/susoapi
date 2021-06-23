# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(comment_question(interview_id = "123"))

})

# TODO: variable_name - add check

# row_vector
test_that("Issues error if `row_vector` is not right content/format", {

    expect_error(
        comment_question(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f", 
            variable_name = "NOM_PRENOMS", 
            roster_vector = "abc", 
            comment = "comment"
        )
    )

})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        comment_question(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            variable_name = "NOM_PRENOMS",
            comment = "testing",
            verbose = TRUE,
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        comment_question(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            variable_name = "NOM_PRENOMS",
            comment = "testing",
            verbose = TRUE,
            workspace = "fake"
        )
    )
})

# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("comment_question_msg", {
        expect_message(
            comment_question(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                variable_name = "NOM_PRENOMS",
                comment = "testing"
            )
        )
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("comment_question_logical", {
        x <- suppressMessages(
            comment_question(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                variable_name = "NOM_PRENOMS",
                comment = "testing",
                verbose = TRUE
            )
        )
    })

    expect_type(x, "logical")

})
