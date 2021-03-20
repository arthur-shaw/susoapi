# inputs

# job_id
test_that("Error if `job_id` not numeric", {
    expect_error(get_export_job_details(job_id = "abc"))
})

# outputs

# message, if details not found
test_that("Message if export job not found", {
    expect_message(get_export_job_details(job_id = 0))
})

# df
test_that("df if export job found", {
    
    vcr::use_cassette("get_export_job_details_df", {
        x <- get_export_job_details(job_id = 1)
    })
    
    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "JobId", "ExportStatus", "StartDate", "CompleteDate", 
        "Progress", "ETA", "Error", "HasExportFile", 
        "ExportType", "QuestionnaireId", "InterviewStatus", "From",
        "To", "AccessToken", "RefreshToken", "StorageType", 
        "TranslationId", "IncludeMeta", "CancelLink", "DownloadLink"
    ), ignore.order = TRUE)

})
