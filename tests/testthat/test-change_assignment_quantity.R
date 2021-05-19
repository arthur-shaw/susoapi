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
