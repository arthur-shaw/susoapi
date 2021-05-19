# INPUTS

# id is a positive integer
test_that("id is a positive integer", {

    expect_error(
        reassign_assignment(
            id = "a",
            responsible = "user123"
        )
    )

})

# responsible is a GUID or user name
test_that("responsible is a GUID or user name", {

    expect_error(
        reassign_assignment(
            id = 2,
            responsible = 123
        )
    )

})

# OUTPUTS

# issues message about success/failure
test_that("issues message about success/failure", {

    expect_message(
        reassign_assignment(
            id = 2,
            responsible = "user123"
        )
    )

})
