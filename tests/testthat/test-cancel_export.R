# inputs

# job_id
test_that("Error if `job_id` not numeric", {
    expect_error(cancel_export(job_id = "abc"))
})

# workspace
# invalid name
test_that("Error if invalid workspace name", {
    expect_error(
        cancel_export(
            job_id = 1,
            workspace = "I am an invalid workspace name",
        )
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        cancel_export(
            job_id = 1,
            workspace = "fake",
        )
    )
})

# outputs

# message
test_that("Message issued about outcome", {
    
    vcr::use_cassette("cancel_export_msg", {
        expect_message(cancel_export(job_id = 1))
    })
    
})

# logical, if verbose = TRUE
test_that("Returns logical if verbose = TRUE", {

    vcr::use_cassette("cancel_export_lgl", {
        expect_message(cancel_export(
            job_id = 1,
            verbose = TRUE))
    })

})
