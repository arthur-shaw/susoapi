# inputs

# user_id
test_that("If invalid `user_id`, issue error", {

    expect_error(
        get_user_action_log(user_id = "123")
    )

})

# start
# TODO: add test once check implemented in function

# end
# TODO: add test once check implemented in function

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_user_action_log(
            user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
            start = "2021-01-01",
            end = "2025-12-30",
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        get_user_action_log(
            user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
            start = "2021-01-01",
            end = "2025-12-30",
            workspace = "I am an invalid workspace name"
        )
    )
})

# outputs

# message
test_that("Returns log", {

    vcr::use_cassette("get_user_action_log_msg", {
        expect_message(
            get_user_action_log(user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895")
        )
    })

})

# log
test_that("Returns log", {

    vcr::use_cassette("get_user_action_log_df", {
        x <- suppressMessages(
            get_user_action_log(
                user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
                start = "2021-01-01",
                end = "2025-12-30"
            )
        )
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c("Time", "Message", "UserId"), ignore.order = TRUE)
    expect_type(x$Time, "character")
    expect_type(x$Message, "character")
    expect_type(x$UserId, "character")

})

