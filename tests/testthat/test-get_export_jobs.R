# inputs

# export_type
test_that("Error issued if `export_type` not in expected set of values", {
    expect_error(get_export_jobs(export_type = "Something completely different"))
})

# interview_status
test_that("Error issued if `interview_status` not in expected set of values", {
    expect_error(get_export_jobs(interview_status = "Something completely different"))
})

# qnr_id
test_that("Error issued if `qnr_id` not in expected format", {
    expect_error(get_export_jobs(qnr_id = "12345$1"))
})

# export_status
test_that("Error issued if `export_status` not in expected format", {
    expect_error(get_export_jobs(export_status = "Something completely different"))
})

# has_file
test_that("Error issued if `has_file` not logical", {
    expect_error(get_export_jobs(has_file = "Something completely different"))
})

# workspace
# invalid form
test_that("Error issued if workspace invalid form", {
    expect_error(get_export_jobs(workspace = "I am an invalid workspace"))
})
# unauthorized or non-existent
test_that("Error issued if workspace invalid form", {
    expect_error(get_export_jobs(workspace = "fake"))
})
# unau

# outputs

# df with expected columns
test_that("Returns df with expected columns", {
    
    vcr::use_cassette("get_export_jobs_df", {
        x <- get_export_jobs()
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "JobId", "ExportStatus", "StartDate", "CompleteDate",
        "Progress", "ETA", "Error", "Links",
        "HasExportFile", "ExportType", "QuestionnaireId", "InterviewStatus",
        "From", "To", "AccessToken", "RefreshToken",
        "StorageType", "TranslationId", "IncludeMeta" 
        ), 
        ignore.order = TRUE)    

})
