# INPUTS

# id is a positive integer
test_that("id is a positive integer", {

    expect_error(
        change_assignment_quantity(id = "abc")
    )

})

# quantity is either -1 or a positive integer
test_that("id is a positive integer", {

    expect_error(
        change_assignment_quantity(
            id = 2,
            quantity = -10
        )
    )

})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {

    expect_error(
        change_assignment_quantity(
            id = 500,
            quantity = -1,
            workspace = "invalid workspace name"
        )
    )

})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {

    expect_error(
        change_assignment_quantity(
            id = 500,
            quantity = -1,
            workspace = "fake"
        )
    )

})

# OUTPUTS

# issues message about success/failure
test_that("issues message about success/failure", {

    expect_message(
        change_assignment_quantity(
            id = 500,
            quantity = -1
        )
    )

})
