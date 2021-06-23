# inputs

# workspace
# invalid workspace
test_that("If invalid workspace, issue error", {

    expect_error(
        get_questionnaire_document(
            workspace = "I am an invalid workspace name",
            qnr = "123",
            qnr_version = 1,
            path = vcr::vcr_test_path("fixtures")
        )
    )

})

# unauthorized or non-existent workspace
test_that("If non-existent or unauthorized workspace, issue error", {

    expect_error(
        get_questionnaire_document(
            workspace = "fake",
            qnr = "123",
            qnr_version = 1,
            path = vcr::vcr_test_path("fixtures")
        )
    )

})

# qnr_id
test_that("If invalid `qnr_id`, issues error", {

    expect_error(
        get_questionnaire_document(
            workspace = "primary",
            qnr = "123",
            qnr_version = 1,
            path = vcr::vcr_test_path("fixtures")
        )
    )

})

# qnr_version
test_that("If invalid `qnr_version`, issues error", {

    expect_error(
        get_questionnaire_document(
            workspace = "primary",
            qnr = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
            qnr_version = "abc",
            path = vcr::vcr_test_path("fixtures")
        )
    )

})

# path
test_that("If invalid `path`, issues error", {

    expect_error(
        get_questionnaire_document(
            workspace = "primary",
            qnr = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
            qnr_version = 1,
            path = "invalid/file/path/"
        )
    )

})

# outputs

# if 200 code...

# ... message

# TODO: check for specific message

test_that("If valid request, issues message", {

    vcr::use_cassette("get_questionnaire_document", {

        expect_message(

            get_questionnaire_document(
                workspace = "primary",
                qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
                qnr_version = 1,
                path = vcr::vcr_test_path("fixtures")
            ),
            "downloaded to"

        )

    })

})

# ... file downloadd

# TODO: check that file downloaded. Tear-down done in setup file

# if non-200 code

# message
