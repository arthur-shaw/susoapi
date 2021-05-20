# INPUTS

# name
test_that("name is valid", {

    expect_error(
        delete_workspace(name = "___")
    )

})

# OUTPUTS

# issues message about success/failure of operation
# NOTE: test always fails with vcr--probably a problem with that package
# vcr::use_cassette("delete_workspace_msg", {
    test_that("issues message about success/failure of operation", {
        
    expect_message(delete_workspace(name = "unittests"))

    })
# })
