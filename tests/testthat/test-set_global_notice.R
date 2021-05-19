# INPUTS

# text is character
test_that("`text` is character", {
    expect_error(
        set_global_notice(
            text = 123
        )
    )
})

# OUTPUTS

# issues message about success/failure of operation
vcr::use_cassette("set_global_notice_msg", {
    test_that("issues message about success/failure of operation", {
        expect_message(
            set_global_notice(text = "This is a unit test.")
        )
    })
})
