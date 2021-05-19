# INPUTS

# OUTPUTS

# if workspace exists, returns df with expected columns
test_that("if workspace exists, returns df with expected columns", {

    # NOTE: test always fails with vcr--must be an isue with that package
    # vcr::use_cassette("get_workspace_status_df", {
        x <- get_workspace_status(name = "primary")
    # })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))

    expect_named(
        object = x,
        expected = c(
            "CanBeDeleted", "WorkspaceName", "WorkspaceDisplayName",
            "ExistingQuestionnairesCount", "InterviewersCount", "SupervisorsCount",
            "MapsCount"
        ),
        ignore.order = TRUE
    )

})

# if workspace does not exist, issues message
# NOTE: test always fails with vcr--must be an isue with that package
# vcr::use_cassette("get_workspace_status_msg", {

    test_that("if workspace does not exist, issues message", {
        expect_message(get_workspace_status(name = "i_do_not_exist"))
    })

# })
