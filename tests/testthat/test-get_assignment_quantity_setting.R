# invalid inputs

# assignment ID
test_that("If invalid `id` parameter, then issues error", {
    expect_error(get_assignment_quantity_setting(id = "boo"))
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_assignment_quantity_setting(
            id = 1,
            workspace = "I am an invalid workspace name"
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        get_assignment_quantity_setting(
            id = 1,
            workspace = "fake"
        )
    )
})

# valid outputs

test_that("Returns a data frame with expected columns", {

    assignment_status <- get_assignment_details(id = 1)
    if (assignment_status[["Archived"]] == TRUE) {
        unarchive_assignment(id = 1)
    } 

    vcr::use_cassette("get_assignment_quantity_setting_returns", {
        x <- get_assignment_quantity_setting(id = 1)
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c("Id", "CanChangeQuantity"), ignore.order = TRUE)
    expect_type(x$Id, "double")
    expect_type(x$CanChangeQuantity, "logical")

})
