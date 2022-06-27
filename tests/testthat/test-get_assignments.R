
# INVALID INPUTS

# TODO: Test these inputs
# search_by
# order
# server
# user
# password

# qnr_id
test_that("Error if invalid questionnaire GUID", {
    expect_error(
        get_assignments(qnr_id = "123")
    )
})

# qnr_version
test_that("Error if invalid questionnaire version", {
    expect_error(
        get_assignments(
            qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
            qnr_version = "a"
        )
    )
})

# responsible
test_that("Error if invalid responsible GUID", {
    expect_error(
        get_assignments(responsible = "a1")
    )
})

# supervisor_id
test_that("Error if invalid supervisor GUID", {
    expect_error(
        get_assignments(supervisor_id = "a1")
    )
})

# show_archive
test_that("Error if non-Boolean value for `show_archive`", {
    expect_error(
        get_assignments(show_archive = "false")
    )
})

# workspace
# invalid workspace name
test_that("Error if invalid workspace name", {
    expect_error(
        get_assignments(workspace = "I am an invalid workspace name")
    )
})
# unauthorized or non-existant workspace
test_that("Error if invalid workspace name", {
    expect_error(
        get_assignments(workspace = "fake")
    )
})

# OUTPUTS

# ... when there are results

test_that("If results, returns a data frame with expected columns", {
    
    vcr::use_cassette("get_assignments_results", {
        x <- get_assignments()
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "Id", "ResponsibleId", "ResponsibleName", 
        "QuestionnaireId", "InterviewsCount", "Quantity", 
        "Archived", "CreatedAtUtc", "UpdatedAtUtc",
        "Email", "Password", "WebMode", 
        "ReceivedByTabletAtUtc", 
        "IsAudioRecordingEnabled"    
    ))
    expect_type(x$Id, "integer")
    expect_type(x$ResponsibleId, "character")
    expect_type(x$ResponsibleName, "character")
    expect_type(x$QuestionnaireId, "character")
    expect_type(x$InterviewsCount, "integer")
    expect_type(x$Quantity, "integer")
    expect_type(x$Archived, "logical")
    expect_type(x$CreatedAtUtc, "character")
    expect_type(x$UpdatedAtUtc, "character")
    # expect_type(x$Email, "character")
    # expect_type(x$Password, "character")
    expect_type(x$WebMode, "logical")
    # expect_type(x$ReceivedByTabletAtUtc, "character")
    expect_type(x$IsAudioRecordingEnabled, "logical")

})

# ...when there are no query results

test_that("If no results, issue a warning", {
    expect_warning(get_assignments(search_by = "no match"))
})

test_that("If no results, returns an empty data frame with expected columns", {
    
    vcr::use_cassette("get_assignments_no_results", {
        x <- suppressWarnings(get_assignments(search_by = "no match"))
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "Id", "ResponsibleId", "ResponsibleName", 
        "QuestionnaireId", "InterviewsCount", "Quantity", 
        "Archived", "CreatedAtUtc", "UpdatedAtUtc",
        "Email", "Password", "WebMode", 
        "ReceivedByTabletAtUtc", "IsAudioRecordingEnabled"    
    ))
    expect_type(x$Id, c("double"))
    expect_type(x$ResponsibleId, "character")
    expect_type(x$ResponsibleName, "character")
    expect_type(x$QuestionnaireId, "character")
    expect_type(x$InterviewsCount, "double")
    expect_type(x$Quantity, "double")
    expect_type(x$Archived, "logical")
    expect_type(x$CreatedAtUtc, "character")
    expect_type(x$UpdatedAtUtc, "character")
    expect_type(x$Email, "character")
    expect_type(x$Password, "character")
    expect_type(x$WebMode, "logical")
    expect_type(x$ReceivedByTabletAtUtc, "character")
    expect_type(x$IsAudioRecordingEnabled, "logical")

})
