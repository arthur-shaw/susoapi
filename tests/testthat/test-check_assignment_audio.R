
# invalid inputs

test_that("If invalid `id` parameter, then issues error", {
    expect_error(check_assignment_audio(id = "boo"))
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {

    expect_error(
        check_assignment_audio(
            id = 1,
            workspace = "invalid workspace name"
        )
    )

})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {

    expect_error(
        check_assignment_audio(
            id = 1,
            workspace = "fake"
        )
    )
    
})

# valid outputs

test_that("Returns a data frame with expected columns", {

    vcr::use_cassette("check_assignment_audio_returns", {
        x <- check_assignment_audio(id = 1)
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c("Id", "Enabled"), ignore.order = TRUE)
    expect_type(x$Id, "double")
    expect_type(x$Enabled, "logical")

})
