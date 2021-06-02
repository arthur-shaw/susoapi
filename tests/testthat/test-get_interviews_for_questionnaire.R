# inputs

# qnr_id
test_that("If invalid `qnr_id`, issues error", {

    expect_error(
        get_interviews_for_questionnaire(
            qnr_id = "123",
            qnr_version = 1
        )
    )

})

# qnr_version
test_that("If invalid `qnr_version`, issues error", {

    expect_error(
        get_interviews_for_questionnaire(
            qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
            qnr_version = "abc"
        )
    )

})

# outputs

# if interviews returned
test_that("If interviews found, return df with expected columns", {

    vcr::use_cassette("get_interviews_for_questionnaire_df", {
        x <- get_interviews_for_questionnaire(
                qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
                qnr_version = 1
            )
    })

    # is data frame
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

# if no interviews returned
# issues a message
test_that("If no interviews found, emit a message", {

    vcr::use_cassette("get_interviews_for_questionnaire_msg", {
        expect_message(get_interviews_for_questionnaire(
                qnr_id = "b4382cca-8231-4e8a-87b0-b1acf8e1ac6c",
                qnr_version = 1
        ))
    })

})

# if interview has no identifying info, still returns df
test_that("if interview has no identifying info, still returns df with expected columns", {

    vcr::use_cassette("get_interviews_for_questionnaire_noid_df", {
        x <- get_interviews_for_questionnaire(
                qnr_id = "5ab793ffc84a4211858df168bbd90cf9",
                qnr_version = 1
            )
    })

    # is data frame
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
