# INPUTS

# OUTPUTS

# issues message
test_that("issues message on success/failure of operation", {

    vcr::use_cassette("update_workspaces_msg", {
        expect_message(
            update_workspace(
                name = "i_do_not_exist",
                display_name = "I do not exist"
            )
        )
    })

})
