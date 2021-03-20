# invalid inputs

test_that("Issues error if `interview_id` is invalid form", {

    expect_error(get_interview_stats(interview_id = "123"))

})

# expected outputs

# issues message

# TODO: use webmockr to mock this outcome

# returns data frame with expected columns

test_that("Returns data frame with expected columns", {

    vcr::use_cassette("get_interview_stats_return", {
        x <- get_interview_stats(interview_id = "7bdf95abab1b4d46b818cdf7546e049f")
    })

    expect_s3_class(x, c("tbl_df","tbl","data.frame"))
    expect_named(x, c(
        "Answered", "NotAnswered", "Flagged", "NotFlagged", "Valid",
        "Invalid", "WithComments", "ForInterviewer", "ForSupervisor",
        "InterviewId", "InterviewKey", "Status", "ResponsibleId",
        "ResponsibleName", "NumberOfInterviewers", "NumberRejectionsBySupervisor",
        "NumberRejectionsByHq", "InterviewDuration", "AssignmentId", "UpdatedAtUtc"
    ))

})
