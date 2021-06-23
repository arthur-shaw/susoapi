# INPUTS

# id is an positive integer
test_that("id is an positive integer", {

    expect_error(
        get_assignment_history(id = "a")
    )

})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {

    expect_error(
        get_assignment_history(
            id = 2,
            workspace = "invalid workspace name"
        )
    )

})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {

    expect_error(
        get_assignment_history(
            id = 2,
            workspace = "fake"
        )
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
