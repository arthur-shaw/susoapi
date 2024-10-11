# uses credentials in args rather than in environment
testthat::test_that("Credentials in args rather than those in env", {

  # set SuSo environment vars as empty, emulatinng situation where
  # - no `.Renviron` file exists and/or
  # - credentials not set
  withr::local_envvar(
    .new = list(
      "SUSO_SERVER" = "",
      "SUSO_WORKSPACE" = "",
      "SUSO_USER" = "",
      "SUSO_PASSWORD" = ""
    )
  )

  testthat::expect_error(
    check_workspace_param(
      server = "https://demo.mysurvey.solutions",
      workspace = "fakespace",
      user = "FakeX1",
      password = "Fake123456"
    ),
    'User `FakeX1` does not have access to workspace `fakespace`.'
  )

})

# returns TRUE when correct credentials provided
testthat::test_that("Returns `TRUE` when correct credentials provided", {

  testthat::expect_true(
    check_workspace_param()
  )

})
