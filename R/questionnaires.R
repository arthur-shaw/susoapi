
#' Count imported questionnaires
#'
#' See how many questionnaires on the server that meet search criteria. Wrapper for \code{GET /api/v1/questionnaires} endpoint
#'
#' @param limit
#' @param offset
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @noRd 
qnr_get_count <- function(
    limit = 40,
    offset = "",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/questionnaires")

    # form the query parameters of the request
    query <- list(
        offset = offset,
        limit = limit
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # get assignments from the server
    response <- httr::GET(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if non-200 code returned, fail loudly
    if (status_code(response) != 200) {
        error_code <- status_code(response)
        stop(
            paste0(
                "Server returned status code ",  error_code,
                ". Check the parameters provided.")
        )
    }

    # otherwise, return the total assignment count
    total_count <- jsonlite::fromJSON(
        content(response, as = "text"),
        flatten = TRUE)$TotalCount
    return(total_count)

}

#' Get batch of questionnaires
#'
#' @param limit
#' @param offset
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @noRd
qnr_get_batch <- function(
    limit = 40,
    offset = "",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/questionnaires")

   # form the query parameters of the request
    query <- list(
        offset = offset,
        limit = limit
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # get questionnaires from the server
    response <- httr::GET(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # return questionnaires
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Questionnaires
    return(df)

}

#' Get all questionnaires
#'
#' Get list of all questionnaires and their attributes
#'
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of questionnaires.
#' 
#' @importFrom purrr map_dfr
#'
#' @export
get_questionnaires <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # get total count of assignments
    total_count <- qnr_get_count(
        offset = 1,
        limit = 1,
        server = server,
        user = user,
        password = password)

    # return all assignments as a dataframe
    df <- purrr::map_dfr(
        .x = seq(
            from = 1,
            to = ceiling(total_count/40),
            by = 1),
        .f = qnr_get_batch,
        limit = 40,
        server = server,
        user = user,
        password = password)

    return(df)

}

# GET ​/api​/v1​/questionnaires​/{id}​/{version}
# ???
# NOTE: Returns list of (all) questionnaires, ignoring {id} and {version}.
# Seems a duplicate of GET ​/api​/v1​/questionnaires

# GET ​/api​/v1​/questionnaires​/{id}​/{version}​/document
# Get JSON representation of questionnaire

#' Save JSON representation of the questionnaire
#'
#' Fetches JSON representation of the questionnaire. Save it to disk. Wrapper for \code{GET ​/api​/v1​/questionnaires​/{id}​/{version}​/document} endpoint
#'
#' @param qnr_id Questionnaire ID. GUID from server
#' @param qnr_version Version number of questionnaire
#' @param path Directory where export JSON representation of the questionnaire should be downloaded
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#' 
#' @import httr
#' @importFrom assertthat assert_that is.count
#' @importFrom fs path
#' 
#' @export
get_questionnaire_document <- function(
    qnr_id,
    qnr_version,
    path,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password        
) {

    # check inputs:
    # qnr_id
    check_guid(
        guid = qnr_id, 
        fail_msg = "Questionnaire ID in `qnr_id` is not a valid GUID.")

    # qnr_version
    assertthat::assert_that(
        assertthat::is.count(qnr_version),
        msg = "Questionnaire version number must be a non-negative integer.")    

    # path
    assertthat::assert_that(
        dir.exists(path), # assertthat::is.dir does not seem to work; using base r replacement
        msg = "Download path specified in `path` is not a valid directory."
    )

    # form the base URL
    base_url <- paste0(server,
        "/api/v1/questionnaires/", qnr_id, "/", qnr_version, "/document")

    # post request and download file
    response <- httr::GET(
        url = base_url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json(),
        write_disk(fs::path(path, "document.json"), overwrite = TRUE)
    )

    # display message about outcome
    if (httr::status_code(response) == 200) {
        message(paste0("Questionnaire downloaded to ", paste0(path, "document.json")))
    } else {
        message("Unable to download the JSON representation of the questionnaire. Unknown reason.")
    }

}

#' Get count of interviews for questionnaire-version
#'
#' Provides count of interviews returned from \code{GET /api/v1/questionnaires/{id}/{version}/interviews} endpoint
#'
#' @param qnr_id Questionnaire ID. GUID from server
#' @param qnr_version Version number of questionnaire
#' @param limit
#' @param offset
#' @param server
#' @param user API user name
#' @param password API password
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @noRd
qnr_get_interview_count <- function(
    qnr_id,
    qnr_version,
    limit = 1,
    offset = 1,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # check inputs:
    # qnr_id
    check_guid(
        guid = qnr_id, 
        fail_msg = "Questionnaire ID in `qnr_id` is not a valid GUID.")

    # qnr_version
    assertthat::assert_that(
        assertthat::is.count(qnr_version),
        msg = "Questionnaire version number must be a non-negative integer.")

    # form the base URL
    base_url <- paste0(server,
        "/api/v1/questionnaires/", qnr_id, "/", qnr_version, "/interviews")

    # form the body for the request
    query <- list(
        limit = limit,
        offset = offset
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # post request
    response <- httr::GET(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # return count of interviews
    total_count <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$TotalCount
    return(total_count)    

}

#' Get list of interviews for questionnaire-version
#'
#' Returns one batch of interviews for a given questionnaire as a df. Wrapper for \code{GET /api/v1/questionnaires/{id}/{version}/interviews} endpoint.
#'
#' @param qnr_id Questionnaire ID. GUID from server
#' @param qnr_version Version number of questionnaire
#' @param limit
#' @param offset
#' @param server
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of interviews
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom tidyr unnest pivot_wider
#' 
#' @noRd
qnr_get_interviews_batch <- function(
    qnr_id,
    qnr_version,
    limit = 40,
    offset = 1,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # qnr_id
    check_guid(
        guid = qnr_id, 
        fail_msg = "Questionnaire ID in `qnr_id` is not a valid GUID.")

    # qnr_version
    assertthat::assert_that(
        assertthat::is.count(qnr_version),
        msg = "Questionnaire version number must be a non-negative integer.")

    # form the base URL
    base_url <- paste0(server,
        "/api/v1/questionnaires/", qnr_id, "/", qnr_version, "/interviews")

    # form the body for the request
    query <- list(
        limit = limit,
        offset = offset
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # post request
    response <- httr::GET(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # return interviews
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Interviews

    # if any interviews returned, transform nested df into tidy df, making FeaturedQuestions into columns
    if (length(df) > 0) {
        df_identifying <- df %>%
            tidyr::unnest(cols = .data$FeaturedQuestions) %>%
            tidyr::pivot_wider(
                id_cols = !matches("^Id$|^Question$|^Answer$"), # all columns except those in nested FeaturesQuestions df
                names_from = .data$Question,
                values_from = .data$Answer
                )
    # else, if no interviews returned, create an empty df
    } else {
        df_identifying <- data.frame(
            InterviewId = NA_character_,
            QuestionnaireId = NA_character_,
            QuestionnaireVersion = NA_real_,
            AssignmentId = NA_real_,
            ResponsibleId = NA_character_,
            ResponsibleName = NA_character_, 
            stringsAsFactors = FALSE
        )
    }

    return(df_identifying)

}

#' Get list of interviews for questionnaire-version
#'
#' Wrapper for \code{GET /api/v1/questionnaires/{id}/{version}/interviews} endpoint
#'
#' @param qnr_id Questionnaire ID. GUID from server.
#' @param qnr_version Questionnaire version number.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of interviews.
#' 
#' @importFrom assertthat assert_that is.count
#' @importFrom purrr map_dfr
#'
#' @export
#' 
#' @examples
get_interviews_for_questionnaire <- function(
    qnr_id,
    qnr_version,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # qnr_id
    check_guid(
        guid = qnr_id, 
        fail_msg = "Questionnaire ID in `qnr_id` is not a valid GUID.")

    # qnr_version
    assertthat::assert_that(
        assertthat::is.count(qnr_version),
        msg = "Questionnaire version number must be a non-negative integer.")

    # form the base URL
    base_url <- paste0(server,
        "/api/v1/questionnaires/", qnr_id, "/", qnr_version, "/interviews")

    # get count of interviews for questionnaire-version
    total_count <- qnr_get_interview_count(
        qnr_id = qnr_id,
        qnr_version = qnr_version,
        server = server,
        user = user,
        password = password)

    # return all interviews as df
    # if no interviews, return an empty df
    if (total_count == 0) {

        message("No interviews available for this questionnaire-version")
        df <- data.frame(
            InterviewId = NA_character_,
            QuestionnaireId = NA_character_,
            QuestionnaireVersion = NA_integer_,
            AssignmentId = NA_integer_,
            ResponsibleId = NA_character_,
            ResponsibleName = NA_character_, 
            stringsAsFactors = FALSE
        )

    # otherwise, return a df with the accumulatio of all server replies
    } else {

        df <- purrr::map_dfr(
            .x = seq(
                from = 1,
                to = ceiling(total_count/40),
                by = 1),
            .f = qnr_get_interviews_batch,
            limit = 40,
            qnr_id = qnr_id,
            qnr_version = qnr_version,
            server = server,
            user = user,
            password = password)
    }

    return(df)

}

# GET ​/api​/v1​/questionnaires​/statuses
# Gets list of possible interview statuses
# NOTE: not worth doing, since don't see value of this endpoint

#' Get possible interview statuses
#' 
#' Wrapper for the `GET ​/api​/v1​/questionnaires​/statuses` endpoint.
#' 
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#' 
#' @return Character vector. Names of all possible interview statuses
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
get_possible_interview_statuses <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/questionnaires/statuses")

    # post request
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )    

    status <- httr::status_code(response)

    # success
    if (status == 200) {

        df <- jsonlite::fromJSON(content(response, as = "text"))
        return(df)

    # unknown error
    } else if (status != 200) {

        message(paste0(
            "Unable to get interview statuses statuses.\n",
            "Reason: unknown error. HTTP code: ", status, "."
        ))

    }

}

#' Enable audio recording for questionnaire
#'
#' Sets audio recording enabled setting for provided questionnaire. Wrapper of \code{POST /api/v1/questionnaires/{id}/{version}/recordAudio} endpoint
#'
#' @param qnr_id Questionnaire ID. GUID from server.
#' @param qnr_version Questionnaire version number.
#' @param enable Whether to enable. Values: c(TRUE, FALSE)
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return TRUE/FALSE depending on whether operation succeeded or not.
#' @export
#'
#' @importFrom assertthat assert_that is.count is.flag
#' @import httr
#' @importFrom jsonlite toJSON
#'
#' @examples
set_questionnaire_audio <- function(
    qnr_id,
    qnr_version,
    enable,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # qnr_id
    check_guid(
        guid = qnr_id, 
        fail_msg = "Questionnaire ID in `qnr_id` is not a valid GUID.")

    # qnr_version
    assertthat::assert_that(
        assertthat::is.count(qnr_version),
        msg = "Questionnaire version number, `qnr_id`, must be a non-negative integer.")

    # enable
    assertthat::assert_that(
        assertthat::is.flag(enable),
        msg = "Whether to enable/disable audio, `enable`, must be a logical value: `TRUE` or `FALSE`")

    # form the base URL
    base_url <- paste0(server,
            "/api/v1/questionnaires/", qnr_id, "/", qnr_version, "/recordAudio")

    # form the body for the request
    audio_val <- ifelse(enable == TRUE, "true", "false")
    body <- list(
        Enabled = audio_val
    )

    # post request
    response <- httr::POST(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # return success (TRUE/FALSE) and, if applicable, and error message.
    status <- httr::status_code(response)
    if (status %in% c(200, 204)) {
        success <- TRUE
        message("Audio setting successfully updated.")
    } else if (status == 404) {
        success <- FALSE
        message("Audio setting not updated. Questionnaire cannot be found.")
    } else {
        success <- FALSE
        message("Audio setting not updated. Unknown reason.")
    }
    return(success)

}
