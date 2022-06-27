# inputs

# export_type
test_that("Error issued if `export_type` not in expected set of values", {
    expect_error(start_export(
        qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1",
        export_type = "Something completely different"))
})

# qnr_id
test_that("Error issued if `qnr_id` not in expected format", {
    expect_error(start_export(
        qnr_id = "12345$1",
        export_type = "STATA"))
})

# interview_status
test_that("Error issued if `interview_status` not in expected set of values", {
    expect_error(start_export(
        qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
        export_type = "STATA",
        interview_status = "Something completely different"))
})

# from
test_that("Error if `from` provided invalid date", {
    expect_error(
        start_export(
            qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
            export_type = "STATA", 
            from = "2020-02-30",
            to = "2020-03-01"
        )
    )
})

# to
test_that("Error if `to` provided invalid date", {
    expect_error(
        start_export(
            qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
            export_type = "STATA", 
            from = "2020-02-22",
            to = "2020-02-30"
        )
    )
})

# storage_type
test_that("Error issued if `storage_type` not in expected set of values", {
    expect_error(start_export(
        qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
        export_type = "STATA",       
        storage_type = "Something completely different"))
})

# translation_id
test_that("Error issued if `translation_id` not in expected format", {
    expect_error(start_export(
        qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
        export_type = "STATA",       
        translation_id = "12345"))
})

# include_meta
test_that("Error issued if `include_meta` not logical", {
    expect_error(start_export(
        qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
        export_type = "STATA",
        include_meta = "I object!"))
})

# workspace
# invalid form
test_that("Error if workspace invalid", {

    expect_error(
        start_export(
            workspace = "I am an invalid workspace name",
            qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
            export_type = "STATA"            
        )
    )

})
# unauthorized or non-existent
test_that("Error if workspace unauthorized or non-existent", {

    expect_error(
        start_export(
            workspace = "fake",
            qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
            export_type = "STATA"           
        )
    )

})

# outputs

# message
test_that("Issue message about outcome of operation", {
    expect_message(start_export(
        qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1", 
        export_type = "STATA"))
})

# job_id
test_that("Return integer job ID", {

    vcr::use_cassette("start_export_job_id", {
        x <- suppressMessages(start_export(
            qnr_id = "5495bfd5f2324b3a8a75c80056f1898e$1",
            export_type = "STATA"))
    })

    expect_type(x, "integer")

})
