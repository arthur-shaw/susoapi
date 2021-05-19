# INPUTS

# OUTPUTS

# issues message
vcr::use_cassette("disable_workspace_msg", {

    test_that("issue message about succes or failure of operation", {
        expect_message(disable_workspace(name = "i_do_not_exist"))
    })

})
