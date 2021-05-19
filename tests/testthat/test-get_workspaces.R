# inputs

# error if user_id is not a GUID
test_that("Error is `user_id` is not a valid GUID", {

    expect_error(get_workspaces(user_id = "abc"))

})

# outputs

# returns df with expected columns
vcr::use_cassette("get_workspaces_df", {
    test_that("Returns df with expected columns", {

        x <- get_workspaces()

        expect_s3_class(x, c("tbl_df","tbl","data.frame"))

        expect_named(
            object = x,
            expected = c("Name", "DisplayName", "DisabledAtUtc"),
            ignore.order = TRUE
        )

    })
})