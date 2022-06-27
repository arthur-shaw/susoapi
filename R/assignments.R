
#' Get the total count of assignments
#'
#' Wrapper for \code{GET /api/v1/assignments} endpoint
#'
#' @param search_by Character. Search for matching text in identifying questions.
#' @param qnr_id Questionnaire ID. GUID provided by the server.
#' @param qnr_version Questionnaire version. Version number provided by the server.
#' @param responsible Character. Either user ID (GUID) or user name.
#' @param supervisor_id Character. User ID (GUID) of supervisor.
#' @param show_archive Include archived assignments. Values: c("true", "false")
#' @param order Possible values are Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc, CreatedAtUtc Followed by ordering direction "ASC" or "DESC"
#' @param offset Character. Name of the workspace whose assignments to get.
#' @param limit Numberic. Number of records to fetch in one request.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Total count of assignments on the server that meet the user-specified search criteria.
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @noRd
get_assignment_count <- function(
    search_by = "",
    qnr_id = "",        # questionnaire ID
    qnr_version = "",   # questionnaire version
    responsible = "",
    supervisor_id = "",
    show_archive = "",  # values: c("true", "false")
    order = "",         # Possible values are Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc, CreatedAtUtc Followed by ordering direction "ASC" or "DESC"
    offset = "",        # integer
    limit = 40,         # integer
    server = Sys.getenv("SUSO_SERVER"),
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # check inputs:

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form the base URL
    base_url <- paste0(server, "/", workspace, "/api/v1/assignments")

    # form the questionnaire ID as QuestionnaireID$Version
    qnr_id_full <- ifelse(
        test = qnr_id != "" & qnr_version != "",
        yes = paste0(qnr_id, "$", qnr_version),
        no = ""
    )

    # form the query parameters of the request
    query <- list(
        SearchBy = search_by,
        QuestionnaireId = qnr_id_full,
        Responsible = responsible,
        SupervisorId = supervisor_id,
        ShowArchive = show_archive,
        Order = order,
        Offset = offset,
        Limit = limit
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # get assignments from the server
    response <- httr::GET(
        url = url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # if non-200 code returned, fail loudly
    if (httr::status_code(response) != 200) {
        error_code <- httr::status_code(response)
        stop(
            paste0(
                "Server returned status code ",  error_code,
                ". Check the parameters provided.")
        )
    }

    # otherwise, return the total assignment count
    total_count <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$TotalCount
    return(total_count)

}


#' Get one batch of assignments
#'
#' @param search_by
#' @param qnr_id
#' @param qnr_version
#' @param responsible
#' @param supervisor_id
#' @param show_archive
#' @param order
#' @param offset
#' @param limit
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of assignments of <= 40 assignments that meet the search criteria.
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @noRd
get_assignment_batch <- function(
    search_by = "",
    qnr_id = "",        # questionnaire ID
    qnr_version = "",   # questionnaire version
    responsible = "",
    supervisor_id = "",
    show_archive = "",  # values: c("true", "false")
    order = "",         # Possible values are Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc, CreatedAtUtc Followed by ordering direction "ASC" or "DESC"
    offset = "",        # integer
    limit = 40,         # integer
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),    
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # TODO: add checks
    # check_guid(qnr_id)
    # check_guid(responsible) | name
    # check_guid(supervisor_id)

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form the base URL
    base_url <- paste0(server, "/", workspace, "/api/v1/assignments")

    # form the questionnaire ID as QuestionnaireID$Version
    qnr_id_full <- ifelse(
        test = qnr_id != "" & qnr_version != "",
        yes = paste0(qnr_id, "$", qnr_version),
        no = ""
    )

    # form the query parameters of the request
    query <- list(
        SearchBy = search_by,
        QuestionnaireId = qnr_id_full,
        Responsible = responsible,
        SupervisorId = supervisor_id,
        ShowArchive = show_archive,
        Order = order,
        Offset = offset,
        Limit = limit
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # get assignments from the server
    response <- httr::GET(
        url = url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # return assignments
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Assignments
    return(df)

}


#' Get all assignments
#'
#' Get all assignments for query parameters. Wrapper for `GET /api/v1/assignments` endpoint.
#'
#' @param search_by Character. Search for matching text in identifying questions.
#' @param qnr_id Questionnaire ID. GUID provided by the server.
#' @param qnr_version Questionnaire version. Version number provided by the server.
#' @param responsible Character. Either user ID (GUID) or user name.
#' @param supervisor_id Character. User ID (GUID) of supervisor.
#' @param show_archive Logical. Include archived assignments.
#' @param order Possible values are Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc, CreatedAtUtc Followed by ordering direction "ASC" or "DESC"
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user User name
#' @param password Password
#'
#' @return Data frame of all assignments that meet search criteria.
#' 
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom purrr map_dfr map_chr
#'
#' @export
get_assignments <- function(
    search_by = "",
    qnr_id = "",        # questionnaire ID
    qnr_version = "",   # questionnaire version
    responsible = "",
    supervisor_id = "",
    show_archive = FALSE, 
    order = "",         # Possible values are Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc, CreatedAtUtc Followed by ordering direction "ASC" or "DESC"
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs

    # qnr_id
    assertthat::assert_that(
        is_guid(qnr_id) | qnr_id == "",
        msg = "Invalid `qnr_id`. The value must be either '' or a valid GUID."
    )

    # qnr_version
    if (qnr_id != "") {
        assertthat::assert_that(
            # when coerced to integer, whether the parameter is non-NA
            !is.na(suppressWarnings(as.integer(qnr_version))),
            msg = "Invalid `qnr_version`. The value must be an integer value expressed as a character"
        )
    }

    # responsible
    if (responsible != "") {
        assertthat::assert_that(
            is_guid(responsible) | is_user_name(responsible), 
            msg = "Responsible ID in `responsible` is not a valid GUID or user name."
        )
    }

    # supervisor_id
    if (supervisor_id != "") {
        assertthat::assert_that(
            is_guid(supervisor_id) | is_user_name(supervisor_id),
            msg = "Supervisor ID in `supervisor_id` is not a valid GUID"
        )
    }

    # search_by
    # TODO: determine what constitutes a valid search. Could simply be any string that has a literal match in identifying questions.

    # show_archive
    assertthat::assert_that(
        assertthat::is.flag(show_archive),
        msg = "Boolean value--`TRUE`/`FALSE`--required for `show_archive`."
    )

    # order
    # columns
    sort_cols <- c("Id", "ResponsibleName", "InterviewsCount", "Quantity", "UpdatedAtUtc", "CreatedAtUtc")
    # sort order
    sort_directions <- c("ASC", "DESC")
    # combine columns and sort order
    sorts_p1 <- purrr::map_chr(.x = sort_cols, .f = ~ paste(.x, sort_directions[1], sep = " "))
    sorts_p2 <- purrr::map_chr(.x = sort_cols, .f = ~ paste(.x, sort_directions[2], sep = " "))
    sorts <- c(sorts_p1, sorts_p2)
    # check whether user inputs is either blank or any of the above
    assertthat::assert_that(
        order %in% c(sort_cols, sorts) | order == "",
        msg = "Error in `order` parameter. Please specify a sort column and, optionally, a sort direction. \nColumns: Id, ResponsibleName, InterviewsCount, Quantity, UpdatedAtUtc, CreatedAtUtc. \nDirections: ASC, DESC")

    # show_archive: convert TRUE/FALSE to "true"/"false"
    show_archive <- dplyr::case_when(
        show_archive == TRUE ~ "true",
        show_archive == FALSE ~ "false"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # get total count of assignments
    total_count <- get_assignment_count(
        search_by = search_by,
        qnr_id = qnr_id,
        qnr_version = qnr_version,
        responsible = responsible,
        supervisor_id = supervisor_id,
        show_archive = show_archive,
        order = order,
        workspace = workspace,
        server = server,
        user = user,
        password = password
    )

    # get all assignments
    df <- purrr::map_dfr(
        .x = seq(
            from = 0,
            to = total_count,
            by = 40),
        .f = get_assignment_batch,
        search_by = search_by,
        qnr_id = qnr_id,
        qnr_version = qnr_version,
        responsible = responsible,
        supervisor_id = supervisor_id,
        show_archive = show_archive,
        order = order,
        workspace = workspace,
        server = server,
        user = user,
        password = password)

    # if no assignments, construct an empty df
    if (total_count == 0) {
        warning("No assignments found that match terms of query.",  call. = FALSE)
        df <- mutate(df, 
            Id = NA_real_,
            ResponsibleId = NA_character_,
            ResponsibleName = NA_character_,
            QuestionnaireId = NA_character_,
            InterviewsCount = NA_real_,
            Quantity = NA_real_,
            Archived = NA,
            CreatedAtUtc = NA_character_,
            UpdatedAtUtc = NA_character_,
            Email = NA_character_,
            Password = NA_character_,
            WebMode = NA,
            ReceivedByTabletAtUtc = NA_character_,
            IsAudioRecordingEnabled = NA
        )
    }

    return(df)

}


# POST ​/api​/v1​/assignments
# Create new assignment

#' Get details for a single assignment
#'
#' Wrapper for \code{GET /api/v1/assignments/{id}} endpoint
#'
#' @param id Integer. Assignment ID
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})server
#' @param workspace Character. Name of the workspace whose assginment details to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of assignment details for single assignment
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom purrr modify_if
#'
#' @export
get_assignment_details <- function(
    id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs

    # assignment ID
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID, `id`, must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(server, "/", workspace, "/api/v1/assignments/", id)

    # get assignments from the server
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # transform details into rectangular data set
    # convert identifying variables from long to wide format df
    id_var <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Id
    identifying_vars_list <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$IdentifyingData
    if (length(identifying_vars_list) > 0) {
        identifying_vars_df <- identifying_vars_list %>%
            dplyr::mutate(Id = id_var) %>%
            tidyr::pivot_wider(id_cols = .data$Id, names_from = .data$Variable, values_from = .data$Answer)

    # if identifying questions empty, create an emty data frame
    } else {

        warning(paste0("No preloaded questions available for assignment ", id_var), call. = FALSE)

        identifying_vars_df <- data.frame(
            Id = id_var
        )

    }

    # retain other variables
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
    other_vars_list <- df[!names(df) %in% c("Answers", "IdentifyingData")]
    other_vars <- purrr::modify_if(.x = other_vars_list, .p = is.null, .f = ~ NA) %>% # replace NULL entries with NA
        bind_rows() # convert from list to data.frame

    # return assignment details
    assign_details <-  left_join(identifying_vars_df, other_vars, by = "Id")
    return(assign_details)

}


# GET ​/api​/v1​/assignments​/{id}​/assignmentQuantitySettings
# Gets Quantity Settings for provided assignment

#' Returns whether assignment quantity can be changed or not
#'
#' Wrapper for \code{GET ​/api​/v1​/assignments​/{id}​/assignmentQuantitySettings} endpoint
#'
#' @param id Assignment ID
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})server
#' @param workspace Character. Name of the workspace whose assginment quantity to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Data frame with `id` and `CanChangeQuantity` columns
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @export
get_assignment_quantity_setting <- function(
    id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

    # check inputs
    # id
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/assignmentQuantitySettings"
    )

    # get assignments from the server
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    response_code <- httr::status_code(response)

    # if 200 code, extract and return the data payload
    if (response_code == 200) {

        df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
        df <- as.data.frame(df)
        df$Id <- id
        return(df)

    # if 404 code, return a message
    } else if (response_code == 404) {
        message(paste0(
            "Cannot get the quantity setting for assignment", id, ".",
            "\nReason: an assignment with this ID either does not exist or is archived."
        ))
    # if another code, return a different warning
    } else {
        message(paste0(
            "Cannot get the quantity setting for assignment", id, ".", 
            "\nReason: unknown. HTTP code: ", response_code
        ))
    }

}

# GET ​/api​/v1​/assignments​/{id}​/history
# Gets history of the assignment

#' Get assignment history
#' 
#' Get the history of actions taken on the target assignment.
#' 
#' Wrapper for the `GET ​/api​/v1​/assignments​/{id}​/history` endpoint
#' 
#' @param id Assignment ID number
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @export 
get_assignment_history <- function(
    id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs
    # id
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(server, "/", workspace, "/api/v1/assignments/", id, "/history")

    # make request
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

    # Assignment cannot accessed by logged in user
    } else if (status == 403) {

        message(paste0(
            "Unable to get assignment history.\n",
            "Reason: User unable to access this assignment. HTTP code: 403."
        ))

    # Assignment cannot be found
    } else if (status == 404) {

        message(paste0(
            "Unable to get assignment history.\n",
            "Reason: Unable to find this assignment. HTTP code: 404."
        ))

    # unknown error
    } else if (!status %in% c(200, 403, 404)) {

        message(paste0(
            "Unable to get assignment history.\n",
            "Reason: Unknown error. HTTP code: ", status, "."
        ))

    }

}

# GET ​/api​/v1​/assignments​/{id}​/recordAudio
# Gets status of audio recording for provided assignment
# TODO: Consider creating a function

#' Gets status of audio recording for provided assignment
#'
#' Informs whether audio audit is enabled for the target assignment. Wrapper for \code{GET ​/api​/v1​/assignments​/{id}​/recordAudio} endpoint
#'
#' @param id Assignment ID number
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Data frame with `id' and `Enabled` columns. `Enabled` is `TRUE` if audio is enabled; `FALSE` if not enabled; `NA` if the request fails for any reason.
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
#' @importFrom jsonlite toJSON
#' 
#' @export
check_assignment_audio <- function(
    id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password     
) {

    # check inputs
    # id
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/recordAudio"
    )

    # get audio from the server
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )


    status <- httr::status_code(response)

    # status returned
    if (status == 200) {

        # extract whether audio enabled
        audio <- fromJSON(content(response, as = "text"))$Enabled

        message(paste0(
            "Audio audit is ", ifelse(audio == TRUE, "", "not "), 
            "enabled for assignment ", id, ".\n",
            "Audio status: "
        ))

    # assignment not found
    } else if (status == 404) {

        audio <- NA

        message(paste0(
            "Assignment ", id, " not found. HTTP code: 404.\n",
            "Audio status: "
        ))

    # other, unexpected result
    } else if (!status %in% c(200, 404)) {

        audio <- NA

        message(paste0(
            "Unable to get the audio recording status of assignment ", id, ". ",
            "HTTP code: ", status, ".\n",
            "Audio status: "
        ))

    }


    df <- data.frame(
        Id = id,
        Enabled = audio
    )

    return(df)

}

#' Set whether assignment will record audio or not
#'
#' Wrapper for \code{PATCH ​/api​/v1​/assignments​/{id}​/recordAudio} endpoint
#'
#' @param id Number. Assignment ID number
#' @param enable Logical. Whether to enable or disable audio--`TRUE` or `FALSE`, respectively
#' @param verbose Logical. Returns information about success of operation: message and `TRUE`/`FALSE` return value.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome: `TRUE` if succeeded; `FALSE` otherwise
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
#' @importFrom jsonlite toJSON
#' 
#' @export
set_assignment_audio <- function(
    id,
    enable,                                 # TRUE or FALSE
    verbose,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password 
) {

    # check inputs
    # id
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/recordAudio"
    )

    # form the body for the request
    audio_val <- ifelse(enable == TRUE, "true", "false")
    body <- list(
        Enabled = audio_val
    )

    # get assignments from the server
    response <- httr::PATCH(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # return success (TRUE/FALSE) and, if applicable, and error message.
    status <- httr::status_code(response)
    if (status %in% c(200, 204)) {
        success <- TRUE
        message(paste0("Audio setting successfully updated for assignment ", id, "."))
    } else if (status == 404) {
        success <- FALSE
        message(paste0("Audio setting not updated for assignment", id, ". Questionnaire cannot be found."))
    } else {
        success <- FALSE
        message(paste0("Audio setting not updated. Unknown reason. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(success)
    }

}

#' Archive an assignment
#'
#' Wrapper for \code{PATCH /api/v1/assignments/{id}/archive} endpoint
#'
#' @param id Numeric. Assignment ID number
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param verbose Logical. Returns information about success of operation: message and `TRUE`/`FALSE` return value.
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome: `TRUE` if succeeded; `FALSE` otherwise
#'
#' @export
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
archive_assignment <- function(
    id,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs
    
    # assignment ID
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(server, "/", workspace, "/api/v1/assignments/", id, "/archive")

    # get assignments from the server
    response <- httr::PATCH(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # if verbose, return whether operation was successful or not        
    result <- httr::status_code(response)

    if (result == 200) {
        success <- TRUE
        message(paste0("Assignment ", id, " successfully archived."))
    } else if (result == 404) {
        success <- FALSE
        message(paste0("Assignment ", id, " not archived. No assignment with this ID can be found."))
    } else {
        success <- FALSE
        message(paste0("Assignment ", id, " not archived. No assignment with this ID can be found. HTTP code: ", result, "."))
    }

    if (verbose == TRUE) {
        return(success)
    }

}

# PATCH ​/api​/v1​/assignments​/{id}​/assign
# Assign new responsible person for assignment
# TODO: Consider making function

#' Reassign assignment to another user
#' 
#' Wrapper for `PATCH ​/api​/v1​/assignments​/{id}​/assign`
#' 
#' @param id Numeric. Assignment ID number
#' @param responsible Character. Either user name or GUID.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#' 
#' @return Server-side side-effect of reassigning an assignment to another user
#' 
#' @importFrom assertthat assert_that is.count is.string
#' @import httr
#' @importFrom jsonlite toJSON
#' 
#' @export 
reassign_assignment <- function(
    id,
    responsible,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")   # API password         
) {

    # check inputs

    # id is a count
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # target_user is a GUID or a user name
    assertthat::assert_that(
        is_guid(responsible) | assertthat::is.string(responsible), 
        msg = "Responsible ID in `responsible` is not a valid GUID."
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/assign"
    )

    # compose body of request
    body <- list(Responsible = responsible)

    # get assignments from the server
    response <- httr::PATCH(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # success
    if (status == 200) {

        message("Assignee for assignment successfully updated")

    # assignment or assignee not found
    } else if (status == 404) {

        message(paste0(
            "Unable to update assignee for assignment.\n",
            "Reason: Assignment or assignee not found. HTTP code: 404"
        ))

    # assignee cannot be assigned an assignment
    } else if (status == 406) {

        message(paste0(
            "Unable to update assignee for assignment.\n",
            "Reason: Assignee cannot be assigned an assignment. HTTP code: 406"
        ))

    # unknown error
    } else if (!status %in% c(200, 404, 406)) {

        message(paste0(
            "Unable to update assignee for assignment.\n",
            "Reason: Unkown error. HTTP code: ", status, "."
        ))

    }

}

# PATCH ​/api​/v1​/assignments​/{id}​/changeQuantity
# Change assignments limit on created interviews

#' Change assignment quantity
#' 
#' Change the quantity for the target assignment.
#' 
#' Wrapper for `PATCH ​/api​/v1​/assignments​/{id}​/changeQuantity`
#' 
#' @param id Numeric. Assignment ID number
#' @param quantity Numeric. New quantity of interviews to collect for this assignment.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#' 
#' @return Server-side side-effect of changing assignment quantity.
#' 
#' @import httr
#' @importFrom assertthat assert_that is.count
#' 
#' @export 
change_assignment_quantity <- function(
    id,
    quantity,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")   # API password         
) {

    # check inputs
    # id
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # quantity
    assertthat::assert_that(
        assertthat::is.count(quantity) | quantity == -1, 
        msg = "Quantity must be either a non-negative integer or -1."
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/changeQuantity"
    )

    # get assignments from the server
    response <- httr::PATCH(
        url = base_url,
        body = as.character(quantity),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # success
    if (status == 200) {
        message("Assignment quantity successfully updated")
    # assignment not found
    } else if (status == 404) {
        message(paste0(
            "Unable to change assignment quantity.\n",
            "Reason: Assignment not found. HTTP code: 404"
        ))
    # size cannot be changed
    } else if (status == 406) {
        message(paste0(
            "Unable to change assignment quantity.\n",
            "Reason: Size cannot be changed. HTTP code: 406"
        ))
    } else if (!status %in% c(200, 404, 406)) {
        message(paste0(
            "Unable to change assignment quantity.\n",
            "Reason: Unknown error. HTTP code: ", status, "."
        ))
    }

}

#' Unarchive an assignment
#'
#' Wrapper for \code{PATCH /api/v1/assignments/{id}/unarchive} endpoint
#'
#' @param id Numeric. Assignment ID number
#' @param verbose Logical. Print whether operation was successful or not
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome: `TRUE` if succeeded; `FALSE` otherwise
#'
#' @export
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
unarchive_assignment <- function(
    id,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")   # API password    
) {

    # check inputs

    # assignment ID
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/unarchive"
    )

    # get assignments from the server
    response <- httr::PATCH(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # if verbose, return whether operation was successful or not 
    result <- httr::status_code(response)

    if (result == 200) {
        success <- TRUE
        message(paste0("Assignment ", id, " successfully unarchived."))
    } else if (result == 404) {
        success <- FALSE
        message(paste0("Assignment ", id, " not unarchived. No assignment with this ID can be found."))
    } else {
        success <- FALSE
        message(paste0("Assignment ", id, " not unarchived. No assignment with this ID can be found. HTTP code: ", result, "."))
    }

    if (verbose == TRUE) {
        return(success)
    }

}


#' Close assignment
#'
#' Closes assignment by setting Size to the amount of collected interviews. Wrapper for \code{PATCH /api/v1/assignments/{id}/close} endpoint
#'
#' @param id Numeric. Assignment ID number
#' @param verbose Logical. Print whether operation was successful or not
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome: `TRUE` if succeeded; `FALSE` otherwise
#'
#' @importFrom assertthat assert_that is.count
#' @import httr
#'
#' @export
close_assignment <- function(
    id,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs

    # assignment ID
    assertthat::assert_that(
        assertthat::is.count(id), 
        msg = "Assignment ID must be a non-negative integer"
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(workspace = workspace)

    # form base URL
    base_url <- paste0(
        server, "/", workspace, 
        "/api/v1/assignments/", id, "/close"
    )

    # get assignments from the server
    response <- httr::PATCH(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # if verbose, return whether operation was successful or not
    result <- httr::status_code(response)

    if (result == 200) {
        success <- TRUE
        message(paste0("Assignment ", id, " successfully closed"))
    } else if (result == 404) {
        success <- FALSE
        message(paste0("Assignment ", id, " not closed. No assignment with this ID can be found"))
    } else if (result == 409) {
        success <- FALSE
        message(paste0("Assignment ", id, " cannot be closed. It is either archived or has web mode enabled."))
    } else {
        success <- FALSE
        message(paste0("Assignment ", id, " not be closed. An unknown issue arose. HTTP code: ", result, "."))
    }

    if (verbose == TRUE) {
        return(success)
    }

}

