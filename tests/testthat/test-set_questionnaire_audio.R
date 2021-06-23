# inputs

# workspace
# invalid workspace
test_that("If invalid workspace, issue error", {

    expect_error(
        set_questionnaire_audio(
            workspace = "I am an invalid workspace",
            qnr_id = "123",
            qnr_version = "abc",
            enable = TRUE
        )        
    )

})

# unauthorized or non-existent workspace
test_that("If non-existent or unauthorized workspace, issue error", {

    expect_error(
        set_questionnaire_audio(
            workspace = "fake",
            qnr_id = "123",
            qnr_version = "abc",
            enable = TRUE
        )        
    )

})

# qnr_id
test_that("If invalid `qnr_id`, issues error", {

    expect_error(
        set_questionnaire_audio(
            workspace = "primary",
            qnr_id = "123",
            qnr_version = "abc",
            enable = TRUE
        )
    )
})

# qnr_version
test_that("If invalid `qnr_version`, issues error", {

    expect_error(
        set_questionnaire_audio(
            workspace = "primary",
            qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
            qnr_version = "abc",
            enable = TRUE
        )
    )
})

# enable
test_that("If invalid `qnr_version`, issues error", {

    expect_error(
        set_questionnaire_audio(
            workspace = "primary",
            qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
            qnr_version = 1,
            enable = "boo"
        )
    )

})

# outputs

# message
test_that("Return message", {

    vcr::use_cassette("set_questionnaire_audio_msg", {
        expect_message(
            set_questionnaire_audio(
                workspace = "primary",
                qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
                qnr_version = 1,
                enable = TRUE
            )            
        )
    })

})

# logical
test_that("Return logical", {

    vcr::use_cassette("set_questionnaire_audio_logical", {
        expect_type(
            suppressMessages(set_questionnaire_audio(
                workspace = "primary",
                qnr_id = "5495bfd5-f232-4b3a-8a75-c80056f1898e",
                qnr_version = 1,
                enable = TRUE
            )), 
            "logical"
        )
    })

})
