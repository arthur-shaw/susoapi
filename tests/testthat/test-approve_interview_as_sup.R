# invalid inputs

# interview ID
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(approve_interview_as_sup(interview_id = "123"))

})

# comment
test_that("Message issued if non-character comment provided", {

    expect_message(
        approve_interview_as_sup(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            comment = 1
        ),
        regexp = "^The value provided in `comment` is not a character, "
    )

})

# verbose
test_that("Issues error if `verbose` is invalid value", {

    expect_error(
        approve_interview_as_sup(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            verbose = "TRUE"
        )
    )

})

# interview ID
test_that("Issues error if `interview_id` is invalid form", {

    expect_error(approve_interview_as_sup(interview_id = "123"))

})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        approve_interview_as_sup(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            comment = "Comment",
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        approve_interview_as_sup(
            interview_id = "7bdf95abab1b4d46b818cdf7546e049f",
            comment = "Comment",
            workspace = "fake"
        )
    )
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

