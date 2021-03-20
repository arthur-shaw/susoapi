# invalid inputs

test_that("If invalid id parameter, then issues error", {
    expect_error(get_assignment_details(id = "boo"))
})

# outputs

test_that("If has identifying ID, returns a data frame with expected columns", {
    
    vcr::use_cassette("get_assignment_details_w_id_vars", {
        x <- get_assignment_details(id = 1)
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))

    expect_type(x$Id, "integer")
    expect_type(x$ResponsibleId, "character")
    expect_type(x$ResponsibleName, "character")
    expect_type(x$InterviewsCount, "integer")
    expect_type(x$Quantity, "integer")
    # expect_type(x$Archived, "logical")
    expect_type(x$CreatedAtUtc, "character")
    expect_type(x$UpdatedAtUtc, "character")
    # expect_type(x$Email, "character")
    # expect_type(x$Password, "character")
    expect_type(x$WebMode, "logical")
    # expect_type(x$ReceivedByTabletAtUtc, "character")
    expect_type(x$IsAudioRecordingEnabled, "logical")

})

test_that("If has no identifying vars, issue warning", {

    vcr::use_cassette("get_assignment_details_wo_ids_warn", {
        expect_warning(get_assignment_details(id = 2))
    })

})

test_that("If has no identifying vars, returns a data frame with expected columns", {
    
    vcr::use_cassette("get_assignment_details_wo_id_vars", {
        x <- suppressWarnings(get_assignment_details(id = 2))
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "Id", "ResponsibleId", "ResponsibleName", 
        "QuestionnaireId", "InterviewsCount", "Quantity", 
        "Archived", "CreatedAtUtc", "UpdatedAtUtc",
        "Email", "Password", "WebMode", 
        "ReceivedByTabletAtUtc", "IsAudioRecordingEnabled"    
    ))

    expect_type(x$Id, "integer")
    expect_type(x$ResponsibleId, "character")
    expect_type(x$ResponsibleName, "character")
    expect_type(x$InterviewsCount, "integer")
    # expect_type(x$Quantity, "integer")
    expect_type(x$Archived, "logical")
    expect_type(x$CreatedAtUtc, "character")
    expect_type(x$UpdatedAtUtc, "character")
    # expect_type(x$Email, "character")
    # expect_type(x$Password, "character")
    expect_type(x$WebMode, "logical")
    # expect_type(x$ReceivedByTabletAtUtc, "character")
    expect_type(x$IsAudioRecordingEnabled, "logical")

})
