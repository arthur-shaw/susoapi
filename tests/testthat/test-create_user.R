# inputs

# role

test_that("Role is Supervisor or Interviewer", {
    expect_error(create_user(
        role = "something else",
        supervisor = "TestInt4R"
    ))
})

# supervisor

test_that("If interviewer, supervisor must be non-empty", {
    expect_error(create_user(
        role = "Interviewer",
    ))
})

# user name
# TODO: Add check that confirms pattern followed

# user password
# TODO: Add check that confirms pattern followed

# outputs

# message
test_that("Displays message on success/failure", {

    vcr::use_cassette("create_user_msg", {
            expect_message(create_user(
                role = "Interviewer", 
                user_name = "abdul134",
                user_password = "Password12345",
                supervisor = "christy094"
            ))
    })

})


# if verbose = TRUE, logical
test_that("Returns logical if verbose = TRUE", {

    vcr::use_cassette("create_user_lgl", {

        suppressMessages(expect_message(create_user(
            role = "Interviewer", 
            user_name = "abdul134",
            user_password = "Password12345",
            supervisor = "christy094",
            verbose = TRUE
        )))       

    })

})
