# INPUTS

# name
test_that("name is valid", {

    expect_error(
        update_workspace(
            name = "___",
            display_name = "I do not exist"
        )
    )

})

# display name
test_that("display name is valid", {

    expect_error(
        update_workspace(
            name = "idonotexist1",
            display_name = ""
        )
    )

})

# OUTPUTS

# issues message
test_that("issues message on success/failure of operation", {

    vcr::use_cassette("update_workspaces_msg", {
        expect_message(
            update_workspace(
                name = "idonotexist1",
                display_name = "I do not exist"
            )
        )
    })

})
