# INPUTS

# interview_id is a GUID
test_that("interview_id is a GUID", {
    expect_error(
        get_interview_transcript(
            interview_id = "abc",
            path = vcr::vcr_test_path("fixtures")
        )
    )
})

# OUTPUTS

# issues message about success/failure
test_that("issues message about success/failure", {

    # NOTE: test always fails with vcr--probably problem with that package
    # vcr::use_cassette("get_interview_transcript_msg", {
        expect_message(
            get_interview_transcript(
                interview_id = "19cb7577-0b1c-48de-b244-31ad29e9d6e4",
                path = vcr::vcr_test_path("fixtures")
            )
        )
    # })

})

# downloads file to indicated path
# TODO: figure out how
