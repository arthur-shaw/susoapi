# credentials empty
test_that("Message issued if credentials empty", {

    withr::local_envvar(.new = list(
        "SUSO_SERVER" = "", 
        "SUSO_WORKSPACE" = "",
        "SUSO_USER" = "", 
        "SUSO_PASSWORD" = ""))

    # suppressMessages(set_credentials(
    #     server = "", 
    #     user = "", 
    #     password = ""
    # ))

    expect_message(check_credentials(), "^Credentials are missing")

})

# credentials wrong
test_that("Message issued if credentials wrong", {

    withr::local_envvar(.new = list(
        "SUSO_SERVER" = "https://demo.mysurvey.solutions", 
        "SUSO_WORKSPACE" = "fakespace",
        "SUSO_USER" = "FakeX1", 
        "SUSO_PASSWORD" = "Fake123456"))

    # suppressMessages(set_credentials(
    #     server = "https://demo.mysurvey.solutions", 
    #     user = "FakeX1", 
    #     password = "Fake123456"
    # ))

    expect_message(check_credentials(), "^Credentials invalid")

})

# credentials wrong and verbose = TRUE
test_that("Returns logical if verbose = TRUE", {

    withr::local_envvar(.new = list(
        "SUSO_SERVER" = "https://demo.mysurvey.solutions", 
        "SUSO_WORKSPACE" = "fakespace",
        "SUSO_USER" = "FakeX1", 
        "SUSO_PASSWORD" = "Fake123456"))

    # suppressMessages(set_credentials(
    #     server = "https://demo.mysurvey.solutions", 
    #     user = "FakeX1", 
    #     password = "Fake123456"
    # ))
    expect_type(
        suppressMessages(check_credentials(verbose = TRUE)), 
        "logical"
    )

})

# credentials can be provided as arguments
test_that("Allows credentials to be provided as args", {

    expect_type(
        suppressMessages(
            susoapi::check_credentials(
                server = "https://demo.mysurvey.solutions",
                workspace = "thatspace",
                user = "FakeX1", 
                password = "Fake123456",
                verbose = TRUE
            )
        ),
        "logical"
    )

})

# credentials provided as arguments override credentials in environment vars
testthat::test_that("Credentials in args override credentials in env vars", {

    withr::local_envvar(.new = list(
        "SUSO_SERVER" = "https://demo.mysurvey.solutions", 
        "SUSO_WORKSPACE" = "fakespace",
        "SUSO_USER" = "FakeX1", 
        "SUSO_PASSWORD" = "Fake123456"
    ))

    # check that the workspace provided in arg appears in message
    testthat::expect_message(

        susoapi::check_credentials(
            server = "https://demo.mysurvey.solutions",
            workspace = "thatspace",
            user = "FakeX1", 
            password = "Fake123456"
        ),
        regexp = "workspace `thatspace`"

    )

})
