# message displayed
test_that("Message issued on function execution", {

    withr::local_envvar(.new = list(
        "SUSO_SERVER" = "https://demo.mysurvey.solutions", 
        "SUSO_USER" = "FakeX1", 
        "SUSO_PASSWORD" = "Fake123456"))

    # suppressMessages(set_credentials(
    #     server = "https://demo.mysurvey.solutions", 
    #     user = "FakeX1", 
    #     password = "Fake123456"
    # ))

    expect_message(show_credentials())

})

# message contains input credentials
test_that("Message contains all input credentials", {

    withr::local_envvar(.new = list(
        "SUSO_SERVER" = "https://demo.mysurvey.solutions", 
        "SUSO_USER" = "FakeX1", 
        "SUSO_PASSWORD" = "Fake123456"))

    # suppressMessages(set_credentials(
    #     server = "https://demo.mysurvey.solutions", 
    #     user = "FakeX1", 
    #     password = "Fake123456"
    # ))


    expect_message(show_credentials(), "https://demo.mysurvey.solutions")
    expect_message(show_credentials(), "FakeX1")
    expect_message(show_credentials(), "Fake123456")

})