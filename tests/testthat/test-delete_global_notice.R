# INPUTS

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        delete_global_notice(workspace = "I am an invalid workspace name")
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        delete_global_notice(workspace = "fake")
    )
})

# OUTPUTS

# issues message about success/failure
vcr::use_cassette("delete_global_notice_msg", {

    test_that("issues message about success/failure", {
        expect_message(
            delete_global_notice()
        )
    })

})
