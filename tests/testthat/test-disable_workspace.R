# INPUTS

# name
test_that("name is valid", {

    expect_error(
        disable_workspace(name = "___")
    )

})

# OUTPUTS

# issues message
vcr::use_cassette("disable_workspace_msg", {

    test_that("issue message about succes or failure of operation", {
        expect_message(disable_workspace(name = "idonotexist"))
    })

})
