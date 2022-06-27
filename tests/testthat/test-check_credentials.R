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
