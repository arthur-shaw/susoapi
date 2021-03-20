# inputs

# user_id, which can take the following forms:
# user ID
# user name
# email

# outputs

# message
test_that("Returns success/failure message", {

    vcr::use_cassette("get_user_details_msg", {
        expect_message(get_user_details(user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895"))
    })

})

# data frame
test_that("Returns df with expected columns", {

    vcr::use_cassette("get_user_details_df", {
        x <- get_user_details(user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895")
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c("IsArchived", "UserId", "UserName", "Role", "IsLocked", "CreationDate"), ignore.order = TRUE)

})

