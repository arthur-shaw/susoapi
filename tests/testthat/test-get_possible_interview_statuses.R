# INPUTS

# N/A

# OUTPUTS

# returns character vector
# NOTE: test always fails with vcr--must be problem with that package
# vcr::use_cassette("get_possible_interview_statuses_chr", {
    test_that("returns character vector", {

            x <- get_possible_interview_statuses()

        expect_type(x, "character")
        
        expect(
            ok = length(x) > 1,
            failure_message = "Should be a non-atomic vector."
        )
    })
# })
