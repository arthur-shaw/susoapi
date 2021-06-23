# inputs

# job_id
test_that("Error if `job_id` invalid format", {
    expect_error(get_export_file(
        job_id = "abc",
        path = vcr::vcr_test_path("fixtures")
    ))
})

# path
test_that("Error if `job_id` invalid format", {
    expect_error(get_export_file(
        job_id = 1,
        path = "invalid/file/path/"
    ))
})

# verbose
# TODO: add check that

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_export_file(
            job_id = 1,
            path = vcr::vcr_test_path("fixtures"),
            workspace = "I am an invalid workspace name"
        )        
    )
})
# unauthorized or non-existant workspace
test_that("Error if unauthorized or non-existant workspace", {
    expect_error(
        get_export_file(
            job_id = 1,
            path = vcr::vcr_test_path("fixtures"),
            workspace = "fake"
        )        
    )
})

# outputs

# message
test_that("Message issued about outcome", {

    vcr::use_cassette("get_export_file_msg", {
        expect_message(get_export_file(
            job_id = 1,
            path = vcr::vcr_test_path("fixtures")
        ))
    })

})

# logical, if verbose = TRUE

# TODO: figure out how to rename file in relative paths or save file with name directly
# test_that("Message issued about outcome", {

#     vcr::use_cassette("get_export_file_msg", {

#         x <- suppressMessages(get_export_file(
#             job_id = 1,
#             path = vcr::vcr_test_path("fixtures"), 
#             verbose = TRUE
#         ))

#     })

#     expect_type(x, "logical")

# })

# file downloaded
# TODO: check that file downloaded. Tear-down done in setup file
