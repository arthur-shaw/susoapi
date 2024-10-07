# inputs

test_that("issues error if invalid workspace name", {

    expect_error(
        get_questionnaires(workspace = "I_am_invalid")
    )

})

testthat::test_that("Accepts URL w/o terminal slash", {

    vcr::use_cassette("get_when_url_does_not_end_in_slash", {
        x <- get_questionnaires(
            server = Sys.getenv("SUSO_SERVER")
        )
    })

    testthat::expect_s3_class(
        x,
        class = c("tbl_df","tbl","data.frame")
    )

})

testthat::test_that("Accepts URL w/ terminal slash", {

    vcr::use_cassette("get_when_url_ends_in_slash", {
        x <- get_questionnaires(
            server = paste0(Sys.getenv("SUSO_SERVER"), "/")
        )
    })

    testthat::expect_s3_class(
        x,
        class = c("tbl_df","tbl","data.frame")
    )

})

# outputs

# returns df with expected columns
test_that("Returns df with expected columns", {

    vcr::use_cassette("get_questionnaires_df", {
        x <- get_questionnaires()
    })

    # is a data frame
    expect_s3_class(x, c("tbl_df","tbl","data.frame"))

    # has expected columns
    expect_named(
        object = x,
        expected = c(
            "questionnaireId", "id", "version", 
            "variable", "title", 
            "defaultLanguageName", "translations"
        ),
        ignore.order = TRUE
    )

    # columns are of expected type
    expect_type(x$id, "character")
    expect_type(x$questionnaireId, "character")
    expect_type(x$version, "integer")
    expect_type(x$variable, "character")
    expect_type(x$title, "character")
    expect_type(x$defaultLanguageName, "character")
    expect_type(x$translations, "list")

})

