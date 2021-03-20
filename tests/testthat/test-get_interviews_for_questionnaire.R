get_interviews_for_questionnaire

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

    vcr::use_cassette("get_interviews_for_questionnaire_w_interviews", {
        x <- get_interviews_for_questionnaire(
                qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
                qnr_version = 1
            )
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_type(x$InterviewId, "character")
    expect_type(x$QuestionnaireId, "character")
    expect_type(x$QuestionnaireVersion, "integer")
    expect_type(x$AssignmentId, "integer")
    expect_type(x$ResponsibleId, "character")
    expect_type(x$ResponsibleName, "character")

})

# if no interviews returned
# issues a message
test_that("If no interviews found, emit a message", {

    vcr::use_cassette("get_interviews_for_questionnaire_no_interviews_message", {
        expect_message(get_interviews_for_questionnaire(
                qnr_id = "5ab793ff-c84a-4211-858d-f168bbd90cf9",
                qnr_version = 1
        ))
    })

})

# returns an empty data frame with expected column names and types
test_that("If no interviews found, return empty df with expected columns", {

    vcr::use_cassette("get_interviews_for_questionnaire_no_interviews_message_df", {
        x <- suppressMessages(get_interviews_for_questionnaire(
                qnr_id = "5ab793ff-c84a-4211-858d-f168bbd90cf9",
                qnr_version = 1
            ))
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "InterviewId", "QuestionnaireId", "QuestionnaireVersion",
        "AssignmentId", "ResponsibleId", "ResponsibleName"
    ), ignore.order = TRUE)
    expect_type(x$InterviewId, "character")
    expect_type(x$QuestionnaireId, "character")
    expect_type(x$QuestionnaireVersion, "integer")
    expect_type(x$AssignmentId, "integer")
    expect_type(x$ResponsibleId, "character")
    expect_type(x$ResponsibleName, "character")

})