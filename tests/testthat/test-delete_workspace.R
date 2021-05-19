# INPUTS

# OUTPUTS

# issues message about success/failure of operation
vcr::use_cassette("delete_workspace_msg", {
    test_that("issues message about success/failure of operation", {
        
    expect_message(delete_workspace(name = "unit_tests"))

    })
})
