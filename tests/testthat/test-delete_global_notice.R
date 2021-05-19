# INPUTS

# OUTPUTS

# issues message about success/failure
vcr::use_cassette("delete_global_notice_msg", {

    test_that("issues message about success/failure", {
        expect_message(
            delete_global_notice()
        )
    })

})
