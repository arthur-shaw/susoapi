# set-up for tests: ensure that interviewers is non-archived
unarchive_user(user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895")

# inputs
# N/A

# outputs

# message(s) for each supervisor
test_that("Returns success/failure message(s) for interviewers", {

    vcr::use_cassette("get_interviewers_msg", {
        expect_message(get_interviewers())
    })

})

# df
test_that("Returns df of supervisors with expected columns", {

    vcr::use_cassette("get_interviewers_df", {
        x <- suppressMessages(get_interviewers())
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "IsArchived", "UserId", "UserName", "Role", "IsLocked", 
        "CreationDate", 
        # "PhoneNumber", "FullName", 
        "SupervisorId", "SupervisorName", "DeviceId"), 
        ignore.order = TRUE)

})
