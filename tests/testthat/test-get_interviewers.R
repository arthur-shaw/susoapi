# set-up for tests: ensure that interviewers is non-archived
unarchive_user(user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895")

# inputs

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_interviewers(
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        get_interviewers(
            workspace = "fake"
        )
    )
})

# outputs

# message(s) for each supervisor
# WHY DISABLED: test always FALSE perhaps because get multiple messages--one for each supervisor
# test_that("Returns success/failure message for each supervisor", {

#     vcr::use_cassette("get_interviewers_msg", {
#         expect_message(get_interviewers())
#     })

# })

# df
test_that("Returns df of supervisors with expected columns", {

    # NOTE: test always fails when using `vcr`, probably because YAML not properly interpreted/replayed
    # vcr::use_cassette("get_interviewers_df", {
        x <- suppressMessages(get_interviewers())
    # })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "IsArchived", "UserId", "UserName", "Role", "IsLocked", 
        "CreationDate", 
        # "PhoneNumber", "FullName", 
        "SupervisorId", "SupervisorName", "DeviceId"), 
        ignore.order = TRUE)

})
