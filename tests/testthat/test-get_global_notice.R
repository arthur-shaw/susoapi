# INPUTS

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_global_notice(
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        get_global_notice(
            workspace = "fake"
        )
    )
})

# OUTPUTS

# issues message about sucess/failure
vcr::use_cassette("get_global_notice_msg", {

    test_that("issues message about sucess/failure", {
        expect_message(get_global_notice())
    })

})

# returns a character or null
test_that("returns a character or null", {

    vcr::use_cassette("get_global_notice_chr", {
        x <- suppressMessages(
            get_global_notice()
        )
    })

    expect(
        ok = (typeof(x) == "character" | typeof(x) == "NULL"),
        failure_message = "Not one of the expected types: character, NULL"
    )

})
