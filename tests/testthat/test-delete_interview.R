# INPUTS

# interview_id is a GUID
test_that("interview_id is a GUID", {

    expect_error(
        delete_interview(interview_id = "abc")
    )

})


# OUTPUTS

# issue message about success or failure of operation
# NOTE: test always fails with vcr--must be problem with that package
# vcr::use_cassette("delete_interview_msg", {

    test_that("issue message about success or failure of operation", {

        expect_message(
            delete_interview(
                interview_id = "19cb7577-0b1c-48de-b244-31ad29e9d6e4"
            )
        )

    })

# })
