# inputs
# N/A

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
