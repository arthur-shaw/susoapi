# inputs

test_that("issues error if invalid workspace name", {

    expect_error(
        get_interviews(workspace = "I_am_fake")
    )

})

# outputs
test_that("returns data frame with expected columns", {

    vcr::use_cassette("get_interviews_df", {
        x <- get_interviews()
    })

    # is a data frame
    expect_s3_class(x, c("tbl_df","tbl","data.frame"))

    # has expected columns
    # names found in data frame, which include interview-specific identifying variables
    col_names <- names(x)
    # expected core values, drawn from GraphQL query
    expected_names <- c(
        "id", "key", "assignmentId", 
        "questionnaireId", "questionnaireVersion", "questionnaireVariable",
        "responsibleName", "responsibleId", "responsibleRole", "supervisorName",
        "status", "actionFlags", "wasCompleted", "notAnsweredCount", "errorsCount",
        "createdDate", "updateDateUtc", "receivedByInterviewerAtUtc", "interviewMode"
    )
    expect_true(all(expected_names %in% col_names))

})
