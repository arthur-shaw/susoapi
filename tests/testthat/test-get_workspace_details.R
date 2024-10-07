# INPUTS

# name
test_that("name is valid", {

    expect_error(
        get_workspace_details(name = "___")
    )

})

# OUTPUTS

# if workspace exists, returns df with expected columns
test_that("if workspace exists, returns df with expected columns", {

    # NOTE: test always fails with vcr--maybe some issue in that package
    # vcr::use_cassette("get_workspace_details_df", {    
        x <- get_workspace_details(name = "primary")
    # })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))

    expect_named(
        object = x,
        expected = c("Name", "DisplayName", "DisabledAtUtc", "CreatedAtUtc"),
        ignore.order = TRUE
    )

})

# if workspace doesn't exist, issues message
test_that("if workspace doesn't exist, issues messag", {

    # vcr::use_cassette("get_workspace_details_msg", {    
    # NOTE: test always fails with vcr--maybe some issue in that package
        expect_message(
            get_workspace_details(name = "idonotexist")
        )
    # })

})
