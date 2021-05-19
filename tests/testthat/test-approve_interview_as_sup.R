# invalid inputs

test_that("Issues error if `interview_id` is invalid form", {

    expect_error(approve_interview_as_sup(interview_id = "123"))

})

# expected outputs

# message

test_that("Issues message if `interview_id` is invalid form", {

    vcr::use_cassette("approve_interview_as_sup_msg", {
        expect_message(
            approve_interview_as_sup(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
                comment = "Comment"
            )
        )
    })

})

# "7bdf95abab1b4d46b818cdf7546e049f"

# logical if `verbose = TRUE`

test_that("Returns logical value if `verbose = TRUE`", {

    vcr::use_cassette("approve_interview_as_sup_logical", {
        x <- suppressMessages(
            approve_interview_as_sup(
                interview_id = "7bdf95abab1b4d46b818cdf7546e049f", 
                comment = "Comment",
                verbose = TRUE
            )
        )
    })

    expect_type(x, "logical")

})

# TODO: investigate outcomes based 
# code 200

# code 404

# code 406

# code not in (200, 404, 406)

