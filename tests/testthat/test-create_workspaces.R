# INPUTS

# TODO: name
# TODO: display_name

# `verbose` is Boolean
test_that("`verbose` is Boolean", {

    expect_error(
        create_workspace(
            name = "myname",
            display_name = "My name",
            verbose = "a"
        )
    )

})

# OUTPUTS

# issue message on success/failure of operation
test_that("issue message on success/failure of operation", {

    expect_message(
        create_workspace(
            name = "unit_tests",
            display_name = "Unit tests"
        )       
    )

})

# return logical if `verbose = TRUE`
test_that("issue message on success/failure of operation", {

    x <- suppressMessages(
        create_workspace(
            name = "unit_tests",
            display_name = "Unit tests",
            verbose = TRUE
        )       
    )

    expect_type(x, "logical")

})
