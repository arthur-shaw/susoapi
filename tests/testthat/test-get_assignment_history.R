# INPUTS

# id is an positive integer
test_that("id is an positive integer", {

    expect_error(
        get_assignment_history(id = "a")
    )

})

# OUTPUTS

# if assignment exists, returns list (FOR NOW)
test_that("if assignment exists, returns list (FOR NOW)", {

    x <- get_assignment_history(id = 2)

    expect_type(x, "list")

})

# if no assignment exists, issues message
test_that("if no assignment exists, issues message", {

    expect_message(
        get_assignment_history(id = 5000)
    )

})
