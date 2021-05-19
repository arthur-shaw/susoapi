# INPUTS

# interview_id is a GUID
test_that("interview_id is a GUID", {

    expect_error(
        get_interview_history(interview_id = "abc")
    )

})

# OUTPUTS

# if interview exists, returns list (FOR NOW)
# NOTE: test always fails with vcr--probably problem with that pakcage
# vcr::use_cassette("get_interview_history_list", {

    test_that("if interview exists, returns list (FOR NOW)", {

        x <- get_interview_history(interview_id = "7bdf95abab1b4d46b818cdf7546e049f")
        expect_type(x, "list")

    })

# })

# if no interview exists, issues message
# NOTE: test always fails with vcr--probably problem with that pakcage
# vcr::use_cassette("get_interview_history_msg", {

    test_that("if no interview exists, issues message", {

        expect_message(
            get_interview_history(interview_id = "19cb7577-0b1c-48de-b244-31ad29e9d6e4")
        )

    })

# })
