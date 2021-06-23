# INPUTS

# text is character
test_that("`text` is character", {
    expect_error(
        set_global_notice(
            text = 123
        )
    )
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        set_global_notice(
            text = "This is a unit test.",
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        set_global_notice(
            text = "This is a unit test.",
            workspace = "fake"
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
