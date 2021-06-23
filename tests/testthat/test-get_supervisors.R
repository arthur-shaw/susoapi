# inputs

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_supervisors(workspace = "I am an invalid workspace name")
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        get_supervisors(workspace = "fake")
    )
})

# outputs

# message(s) for each supervisor
test_that("Returns success/failure message(s) for supervisors", {

    # NOTE: test always fails when using VCR--perhaps because YAML poorly interpretted/replayed
    # vcr::use_cassette("get_supervisors_msg", {
        expect_message(get_supervisors())
    # })

})

# df
test_that("Returns df of supervisors with expected columns", {

    # NOTE: test always fails when using VCR--perhaps because YAML poorly interpretted/replayed
    # vcr::use_cassette("get_supervisors_df", {
        x <- suppressMessages(get_supervisors())
    # })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "IsArchived", "UserId", "UserName", "Role", "IsLocked", 
        "CreationDate", 
        # "PhoneNumber", "FullName", "Email", 
        "DeviceId"), 
        ignore.order = TRUE)

})
