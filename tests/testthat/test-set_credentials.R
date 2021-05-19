# message displayed
# test_that("Message issued after function executed", {

#     expect_message(
#         set_credentials(
#             server = "https://demo.mysurvey.solutions", 
#             user = "FakeX1", 
#             password = "Fake123456"
#         )        
#     )

# })

# credentials set to environment variables
# test_that("Credentials set to environment variables", {

#     suppressMessages(set_credentials(
#         server = "https://demo.mysurvey.solutions", 
#         user = "FakeX1", 
#         password = "Fake123456"
#     ))    

#     expect_equal(Sys.getenv("SUSO_SERVER"), "https://demo.mysurvey.solutions")
#     expect_equal(Sys.getenv("SUSO_USER"), "FakeX1")
#     expect_equal(Sys.getenv("SUSO_PASSWORD"), "Fake123456")

# })
