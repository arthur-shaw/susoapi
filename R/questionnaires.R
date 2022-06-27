
#' Get all questionnaires
#'
#' Get list of all questionnaires and their attributes
#' 
#' GraphQL implementation of the deprecated REST `GET​/api​/v1​/questionnaires` endpoint.
#'
#' @param workspace Character. Name of the workspace whose questionnaires to get.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of questionnaires.
#' 
#' @importFrom assertthat assert_that
#' @import ghql
#' @importFrom jsonlite base64_enc fromJSON
#' @importFrom glue glue double_quote
#' @importFrom dplyr pull
#'
#' @export
get_questionnaires <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs
    # invalid name
    # workspace does not exist
    check_workspace_param(workspace = workspace)

    # compose the GraphQL request client
    questionnaires_request <- ghql::GraphqlClient$new(
        url = paste0(server, "/graphql"), 
        headers = list(authorization = paste0(
            "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
        )
    )

    # compose the query for all interviews
    # use string interpolation to pipe double-quoted workspace name into query
    qry <- ghql::Query$new()
    qry$query("questionnaires", 
        glue::glue("{
            questionnaires (workspace: <glue::double_quote(workspace)>) {
                nodes {
                    id
                    questionnaireId
                    version
                    variable
                    title
                    defaultLanguageName
                    translations {
                        id
                        name
                    }
                }
                filteredCount   
            }
        }", .open = "<", .close = ">")
    )

    # send request
    questionnaires_result <- questionnaires_request$exec(qry$queries$questionnaires)

    # convert JSON payload into an R object
    qnrs <- jsonlite::fromJSON(questionnaires_result, flatten = TRUE)
    qnr_count <- qnrs$data$questionnaires$filteredCount

    if ("errors" %in% names(qnrs)) {

        # extract and display error(s)
        errors <- dplyr::pull(qnrs$errors) %>% paste0(collapse = "\n")
        stop(errors)

    } else if (qnr_count == 0) {

        message(glue::glue(
            "No questionnaires found in workspace {glue::backtick(workspace)}.",
            "If this result is surprising, check the input in the `workspace` parameter.",
            .sep = "\n"
        ))

    } else if (qnr_count > 0) {

        # extract data frame from nested containers
        qnrs_df <- qnrs$data$questionnaires$nodes

        # correct class of defaultLanguageName, which may often be empty
        qnrs_df$defaultLanguageName <- as.character(qnrs_df$defaultLanguageName)

        # rename variables to names from REST ?

            # What REST CURRENTLY RETURNS:
            # "QuestionnaireIdentity": "string",
            # "QuestionnaireId": "3fa85f64-5717-4562-b3fc-2c963f66afa6",
            # "Version": 0,
            # "Title": "string",
            # "Variable": "string",
            # "LastEntryDate": "2021-06-01T13:41:59.328Z",
            # "IsAudioRecordingEnabled": true,
            # "WebModeEnabled": true

            # How to rename:
            # qnrs_df <- qnrs_df %>%
            #     rename(
            #         QuestionnaireIdentity = questionnaireId,
            #         QuestionnaireId = id,
            #         Version = version,
            #         Variable = variable,
            #         Title = title
            #     )

        return(qnrs_df)

    }

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
#' @param workspace Character. Name of the workspace whose questionnaire document to get.
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
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password        
) {

    # check inputs:

    # workspace
    check_workspace_param(workspace = workspace)
    
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
    base_url <- paste0(
        server,
        "/", workspace,
        "/api/v1/questionnaires/", qnr_id, "/", qnr_version, "/document"
    )

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
#' @param workspace Character. Name of the workspace whose interviews to get.
#' @param qnr_id Questionnaire ID. GUID from server.
#' @param qnr_version Questionnaire version number.
#' @param workspace Character. Name of the workspace whose interviews to get.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Charater. API or admin user name for user that access to the workspace.
#' @param password API or admin password
#' 
#' @return List consisting of two element: interviews information and interview count
#' 
#' @import ghql
#' @importFrom jsonlite base64_enc fromJSON
#' @importFrom glue glue double_quote
#' 
#' @noRd 
get_interviews_for_questionnaire_count <- function(
    qnr_id,
    qnr_version,    
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # compose the GraphQL request client
    interviews_request <- ghql::GraphqlClient$new(
        url = paste0(server, "/graphql"), 
        headers = list(authorization = paste0(
            "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
        )
    )

    # compose the query for all interviews
    # use string interpolation to pipe double-quoted workspace name into query
    qry <- ghql::Query$new()
    qry$query("interviews", 
        glue::glue("{
            interviews (
                workspace: <glue::double_quote(workspace)>,
                where: {
                    questionnaireId: {eq: <glue::double_quote(qnr_id)>}
                    questionnaireVersion: {eq: <qnr_version>}
                }
                take: 1
                skip: 0
            ) {
                filteredCount
            }
        }", .open = "<", .close = ">")
    )

    # send request
    interviews_result <- interviews_request$exec(qry$queries$interviews)

    # convert JSON payload to data frame
    interviews <- jsonlite::fromJSON(interviews_result, flatten = TRUE)

    # extract total number of interviews
    interviews_count <- interviews$data$interviews$filteredCount

    interview_info <- list(interviews = interviews, interviews_count = interviews_count)

    return(interview_info)

}

#' Get chuck of interviews returned from the server for the questionnaire-version
#' 
#' @param workspace Character. Name of the workspace whose interviews to get.
#' @param take_n Numeric. Number of interviews to take in one request.
#' @param skip_n Numeric. Number of interviews to skip when paging through results.
#' @param qnr_id Questionnaire ID. GUID from server.
#' @param qnr_version Questionnaire version number.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Charater. API or admin user name for user that access to the workspace.
#' @param password API or admin password
#' 
#' @return Data frame. Interviews.
#' 
#' @import ghql
#' @importFrom jsonlite base64_enc fromJSON
#' @importFrom glue glue double_quote backtick
#' @importFrom dplyr `%>%` pull select rename_with starts_with left_join
#' @importFrom purrr map_if discard map_int
#' @importFrom rlang .data is_empty
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest pivot_wider
#' 
#' @noRd 
get_interviews_for_questionnaire_by_chunk <- function(
    take_n = 100,
    skip_n = 0,
    qnr_id,
    qnr_version,    
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # compose the GraphQL request client
    interviews_request <- ghql::GraphqlClient$new(
        url = paste0(server, "/graphql"), 
        headers = list(authorization = paste0(
            "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
        )
    )

    # compose the query for all interviews
    # use string interpolation to pipe double-quoted workspace name into query
    qry <- ghql::Query$new()
    qry$query("interviews", 
        glue::glue("{
            interviews (
                workspace: <glue::double_quote(workspace)>,
                where: {
                    questionnaireId: {eq: <glue::double_quote(qnr_id)>}
                    questionnaireVersion: {eq: <qnr_version>}
                }
                take: <take_n>
                skip: <skip_n>
            ) {
                nodes {
                    id
                    key
                    assignmentId
                    identifyingData {
                        answerValue
                        value
                        valueBool
                        valueDate
                        valueLong
                        valueDouble
                        isEnabled
                        entity {
                            identifying
                            label
                            options {
                                parentValue
                                title
                                value
                            }
                            questionText
                            scope
                            type
                            variable
                        }
                    }
                    questionnaireId
                    questionnaireVersion
                    questionnaireVariable
                    responsibleName
                    responsibleId
                    responsibleRole
                    supervisorName
                    status
                    actionFlags
                    wasCompleted
                    notAnsweredCount
                    errorsCount
                    createdDate
                    updateDateUtc
                    receivedByInterviewerAtUtc
                    interviewMode        
                }
                filteredCount
            }
        }", .open = "<", .close = ">")
    )

    # send request
    interviews_result <- interviews_request$exec(qry$queries$interviews)

    # convert JSON payload to data frame
    interviews <- jsonlite::fromJSON(interviews_result, flatten = TRUE)
    
    interviews_count <- interviews$data$interviews$filteredCount    

    if ("errors" %in% names(interviews)) {

        # extract and display error(s)
        errors <- dplyr::pull(interviews$errors) %>% paste0(collapse = "\n")
        stop(errors)

    } else if (interviews_count == 0) {

        message(glue::glue(
            "No interviews found for these search parameters:",
            "- `workspace`: {workspace}",
            "- `qnr_id`: {qnr_id}",
            "- `qnr_version`: {qnr_version}",
            "If this result is surprising, check the search parameter.",
            .sep = "\n"
        ))

    } else if (interviews_count > 0) {

        # extract interview data payload
        interviews_df <- interviews$data$interviews$nodes %>% 
            purrr::map_if(is.data.frame, list) %>% 
            tibble::as_tibble()

        # extract interview attributes from the payload
        interview_attribs_df <- dplyr::select(interviews_df, -.data$identifyingData)

        # determine whether contains any identifying data
        # compute the length of identifying data df for each record
        has_identifying <- interviews_df %>%
            dplyr::select(id, .data$identifyingData) %>%
            dplyr::mutate(has_identifying = purrr::map_int(.data$identifyingData, length))
        # create summary measure whether any obs has identifying
        has_any_identifying <- any(has_identifying$has_identifying > 0)

        if (has_any_identifying == TRUE) {

            # extract (nested) identifying data
            identifying_df <- interviews_df %>% 
                dplyr::select(id, .data$identifyingData) %>%
                purrr::discard(rlang::is_empty) %>%
                purrr::map_if(is.data.frame, list) %>% 
                tibble::as_tibble() %>%
                tidyr::unnest(.data$identifyingData) %>%
                dplyr::rename_with(
                    .cols = starts_with("entity."),
                    .fn = ~ gsub(
                        pattern = "entity.",
                        replacement = "",
                        x = .x
                    )
                ) %>%
                dplyr::select(id, .data$value, .data$variable) %>%
                tidyr::pivot_wider(
                    id_cols = id,
                    names_from = .data$variable,
                    values_from = .data$value
                )

            # combine interview attributes and identifying data
            interview_list_df <- interview_attribs_df %>%
                dplyr::left_join(identifying_df, by = "id")

        } else if (has_any_identifying == FALSE) {

            interview_list_df <- interview_attribs_df

        }

        return(interview_list_df)

    }

}

#' Get list of interviews for questionnaire-version
#'
#' GraphQL implmentation for deprecated REST \code{GET /api/v1/questionnaires/{id}/{version}/interviews} endpoint
#'
#' @param workspace Character. Name of the workspace whose questionnaires and associated interviews to get.
#' @param chunk_size Numeric. Number of records to take in one request.
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
get_interviews_for_questionnaire <- function(
    chunk_size = 100,
    qnr_id,
    qnr_version,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
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

    # get total count of interviews
    interviews_info <- get_interviews_for_questionnaire_count(
        workspace = workspace, 
        qnr_id = qnr_id,
        qnr_version = qnr_version,  
        server = server, 
        user = user, 
        password = password
    )

    # case 1: handle "errors"
    # if request returns errors
    if ("errors" %in% names(interviews_info$interviews)) {

        # extract and display error(s)
        errors <- dplyr::pull(interviews_info$interviews$errors) %>% paste0(collapse = "\n")
        stop(errors)

    # if no interviews found
    } else if (interviews_info$interviews_count == 0) {

        message(glue::glue(
            "No interviews found for these search parameters:",
            "- `workspace`: {workspace}",
            "- `qnr_id`: {qnr_id}",
            "- `qnr_version`: {qnr_version}",
            "If this result is surprising, check the search parameter.",
            .sep = "\n"
        ))

    # case 2: handle interviews
    } else if (interviews_info$interviews_count > 0) {

        # page through interviews
        interviews <- purrr::map_dfr(
            .x = seq(from = 0, to = interviews_info$interviews_count, by = chunk_size),
            .f = ~ get_interviews_for_questionnaire_by_chunk(
                workspace = workspace,
                take_n = chunk_size,
                skip_n = .x,
                qnr_id = qnr_id,
                qnr_version = qnr_version,                 
                server = server, 
                user = user, 
                password = password            
            )
        )

        return(interviews)

    }

}

# GET ​/api​/v1​/questionnaires​/statuses
# Gets list of possible interview statuses

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
#' @param workspace Character. Name of the workspace whose questionnaire audio settings to change.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#' @param enable Logical. Whether to enable. Values: c(TRUE, FALSE)
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
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # workspace
    check_workspace_param(workspace = workspace)

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
            "/", workspace,
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
