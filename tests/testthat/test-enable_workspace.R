# INPUTS

# name
test_that("name is valid", {

    expect_error(
        enable_workspace(name = "___")
    )

})

# OUTPUTS

# issues message about success/failure of operation
vcr::use_cassette("enable_workspace_msg", {

    test_that("issues message about success/failure of operation", {
        expect_message(enable_workspace(name = "idonotexist"))
    })

})
