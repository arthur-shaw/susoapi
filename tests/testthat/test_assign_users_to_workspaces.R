# INPUTS

# users is a character vector
test_that("users is a character vector", {

    expect_error(
        assign_users_to_workspaces(
            users = 1,
            workspaces = "primary"
        )
    )

})

# each element of users is a GUID
test_that("each element of users is a GUID", {

    expect_error(
        assign_users_to_workspaces(
            users = c("7bdf95abab1b4d46b818cdf7546e049f", "abc"),
            workspaces = "primary"
        )
    )

})

# workspaces is a character vector
test_that("workspaces is a character vector", {

    expect_error(
        assign_users_to_workspaces(
            users = "7bdf95abab1b4d46b818cdf7546e049f",
            workspaces = 1
        )
    )

})

# workspaces is composed of all valid names
test_that("workspaces is a character vector", {

    expect_error(
        assign_users_to_workspaces(
            users = "7bdf95abab1b4d46b818cdf7546e049f",
            workspaces = "this is wrong"
        )
    )

})

# action %in% c("Assign", "Add", "Remove")
test_that('action %in% c("Assign", "Add", "Remove")', {

    expect_error(
        assign_users_to_workspaces(
            users = "7bdf95abab1b4d46b818cdf7546e049f",
            workspaces = "primary",
            action = "Nothing"
        )
    )

})

# OUTPUTS

# issues message on success/failure of operation
vcr::use_cassette("assign_users_to_workspaces_msg", {
    test_that("issues message on success/failure of operation", {

        expect_message(
            assign_users_to_workspaces(
                users = "7bdf95abab1b4d46b818cdf7546e049f",
                workspaces = "primary",
                action = "Assign"
            )
        )

    })
})
