# invalid inputs

# interview_id
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(assign_interview_to_int(
                    interview_id = "123",
                    user_name = "ArthurInt123"
                ))

})

# user_id
test_that("Issues error if `user_id` is invalid form", {

    expect_error(assign_interview_to_int(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f",    
                    user_id = "123"
                ))

})

# user_name
test_that("Issues error if `user_id` and `user_name` empty", {

    expect_error(assign_interview_to_int(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f"
                ))

})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        assign_interview_to_int(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            user_name = "ArthurInt123",
            verbose = TRUE,
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        assign_interview_to_int(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            user_name = "ArthurInt123",
            verbose = TRUE,
            workspace = "fake"
        )
    )
})

# expected outputs

# message
test_that("Issues message to inform about outcome", {

    vcr::use_cassette("assign_interview_to_int_msg", {
        expect_message(
            assign_interview_to_int(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                user_name = "ArthurInt123",
                verbose = TRUE
            )
        )
    })

})

# logical if `verbose = TRUE`
test_that("Returns logical if `verbose = TRUE`", {

    vcr::use_cassette("assign_interview_to_int_logical", {
        suppressMessages(
            expect_message(
                assign_interview_to_int(
                    interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                    user_name = "ArthurInt123",
                    verbose = TRUE
                )
            )
        )
    })

})
