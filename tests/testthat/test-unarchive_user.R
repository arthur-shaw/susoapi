# inputs

# user_id
test_that("Issues error if `user_id` is not GUID", {

    expect_error(unarchive_user(user_id = "123"))

})

# verbose
test_that("Error if invalid workspace name", {
    expect_error(
        unarchive_user(
            user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
            verbose = "TRUE"
        )
    )
})


# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        unarchive_user(
            user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
            verbose = TRUE,
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        unarchive_user(
            user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
            verbose = TRUE,
            workspace = "fake"
        )
    )
})

# outputs

# message
test_that("Returns message about success/failure", {

    vcr::use_cassette("unarchive_user_msg", {
        expect_message(unarchive_user(user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895"))
    })

})

# if verbose = TRUE, logical
test_that("Returns logical if verbose = TRUE", {

    vcr::use_cassette("unarchive_user_lgl", {

        expect_type(
            object = suppressMessages(
                unarchive_user(
                    user_id = "cc78158d-e987-4d7e-9cfb-fb4546d2c895", 
                    verbose = TRUE
                )
            ),
            type = "logical"
        )

    })

})
