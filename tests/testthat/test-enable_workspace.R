# INPUTS

# OUTPUTS

# issues message about success/failure of operation
vcr::use_cassette("enable_workspace_msg", {

    test_that("issues message about success/failure of operation", {
        expect_message(enable_workspace(name = "i_do_not_exist"))
    })

})
