
# invalid inputs

test_that("If invalid `id` parameter, then issues error", {
    expect_error(check_assignment_audio(id = "boo"))
})

# valid outputs

test_that("Returns a data frame with expected columns", {

    vcr::use_cassette("check_assignment_audio_returns", {
        x <- check_assignment_audio(id = 3420)
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c("Id", "Enabled"), ignore.order = TRUE)
    expect_type(x$Id, "double")
    expect_type(x$Enabled, "logical")

})
