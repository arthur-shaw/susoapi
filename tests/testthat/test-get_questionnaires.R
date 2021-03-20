# inputs

# N/A

# outputs

# returns df with expected columns
test_that("Returns df with expected columns", {

    vcr::use_cassette("get_questionnaires_return", {
        x <- get_questionnaires()
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "QuestionnaireIdentity", "QuestionnaireId", "Version",
        "Title", "Variable", "LastEntryDate",
        "WebModeEnabled", "IsAudioRecordingEnabled"
    ), ignore.order = TRUE)
    expect_type(x$QuestionnaireIdentity, "character")
    expect_type(x$QuestionnaireId, "character")
    expect_type(x$Version, "integer")
    expect_type(x$Title, "character")
    expect_type(x$Variable, "character")
    expect_type(x$LastEntryDate, "character")

})

