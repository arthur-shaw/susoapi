# GET ​/api​/v1​/interviews​/{id}
# Gets all the answers for given interview

# DELETE ​/api​/v1​/interviews​/{id}
# Deletes interview

# GET ​/api​/v1​/interviews​/{id}​/history
# Get interivew history for interview (?)

#' Get statistics by interview
#'
#' Fetch statistics for a single interview. 
#' 
#' These statistics provide metadata about the interview process. Columns include: `Answered`, `NotAnswered`, `Flagged`, `NotFlagged`, `Valid`, `Invalid`, `WithComments`, `ForInterviewer`, `ForSupervisor`, `InterviewId`, `InterviewKey`, `Status`, `ResponsibleId`, `ResponsibleName`, `NumberOfInterviewers`, `NumberRejectionsBySupervisor`, `NumberRejectionsByHq`, `InterviewDuration`, `AssignmentId`, `UpdatedAtUtc`. 
#' 
#' Wrapper for \code{GET /api/v1/interviews/{id}/stats} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame with all values provided by the server. 
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @export
get_interview_stats <- function(
    interview_id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/stats")

    # get stats from the server
    response <- httr::GET(
        url = base_url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # return data payload
    result <- httr::status_code(response)
    # if request successful, then put stats in a df
    if (result == 200) {

        df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
        df <- as.data.frame(df)

    # if request not successful--or just results in unexpected code--then create an empty df with expected column names
    } else if (result != 200) {

        message(paste0(
            "Failed to obtain interview status for interview ", interview_id, 
            ". \nCheck the interview__id. HTTP code: ", result, "."))

        df <- data.frame(
            Answered = NA_real_, 
            NotAnswered = NA_real_, 
            Flagged = NA_real_, 
            NotFlagged = NA_real_, 
            Valid = NA_real_, 
            Invalid = NA_real_, 
            WithComments = NA_real_, 
            ForInterviewer = NA_real_, 
            ForSupervisor = NA_real_, 
            InterviewId = interview_id, 
            InterviewKey = NA_character_, 
            Status = NA_character_, 
            ResponsibleId = NA_character_, 
            ResponsibleName = NA_character_, 
            NumberOfInterviewers = NA_real_, 
            NumberRejectionsBySupervisor = NA_real_, 
            NumberRejectionsByHq = NA_real_, 
            InterviewDuration = NA_character_, 
            AssignmentId = NA_real_, 
            UpdatedAtUtc = NA_character_ ,
            stringsAsFactors = FALSE
        )
    }

    return(df)

}

#' Approve interview as supervisor
#'
#' Has the server-side side-effect of approving a single interview on the server as a supervisor. For interactive use, prints a message to describe the outcome of the action. If \code{verbose = TRUE}, returns a logical value about the outcome that may be useful for functions that need to track successes and failures. 
#' 
#' Wrapper for \code{PATCH /api/v1/interviews/{id}/approve} endpoint.
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param comment Comment to post upon approval.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome.
#'
#' @import httr
#'
#' @export
approve_interview_as_sup <- function(
    interview_id,
    comment = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/approve")

    # form query portion of request
    query <- list(
        id = interview_id,
        comment = comment
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # get assignments from the server
    response <- httr::PATCH(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message
    status <- httr::status_code(response)

    # approved
    if (status == 200) {
        result <- TRUE
        message(paste0("Interview ", interview_id, " successfully approved. "))            
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not approved. ",
            "\nReason: Interview not found."))
    # cannot be approved because of status
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not approved. ",
            "\nReason: Interview not in status to be approved."))
    # unknown outcome
    } else if (!status %in% c(200, 404, 406)) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not approved. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)
    }

}

#' Reassign interview to interviewer
#'
#' Has the server-side side-effect of reassigning a single interview to a target interviewer. For interactive use, prints a message to describe the outcome of the action. If \code{verbose = TRUE}, returns a logical value about the outcome that may be useful for functions that need to track successes and failures. 
#' 
#' Wrapper for \code{PATCH /api/v1/interviews/{id}/assign} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param user_id User ID. GUID from server.
#' @param user_name User name of target interviewer.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome.
#' 
#' @export
#'
#' @import httr
#' @importFrom jsonlite toJSON
#'
#' @examples
assign_interview_to_int <- function(
    interview_id,
    user_id = "",
    user_name = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # check that either user ID or user name provided
    if (user_id == "" & user_name == "") {
        stop("Either `user_id` or `user_name` must be provided.")
    }

    # if user_id provided, check that it is a valid guid
    if (user_id != "") {
        check_guid(
            guid = user_id, 
            fail_msg = "User ID in `user_id` is not a valid GUID.")
    }

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/assign")

    # form the body for the assignment request
    # ... if only `user_id` provided
    if (user_id != "" & user_name == "") {
        body <- list(
            ResponsibleId = user_id
        )
    # ... if only `user_name` provided
    } else if (user_id == "" & user_name != "") {
        body <- list(
            ResponsibleName = user_name
        )
    # ... if both `user_id` and `user_name` provided
    } else if (user_id != "" & user_name != "") {
        body <- list(
            ResponsibleId = user_id,
            ResponsibleName = user_name
        )
    }

    # post request
    response <- httr::PATCH(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose, return success (TRUE/FALSE) and, if applicable and message.

    status <- httr::status_code(response)

    # if interview assigned
    if (status == 200) {
        success <- TRUE
        message(paste0("Interview ", interview_id, " successfully reassigned. "))            
    # if interview not found
    } else if (status == 404) {
        success <- FALSE
        message(paste0("Interview ", interview_id, " not reassigned. ",
            "\nReason: Interview not found."))
    # if interview cannot be reassigned
    } else if (status == 406) {
        success <- FALSE
        err_msg <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
        err_msg <- ifelse(
            test = !is.na(err_msg), 
            yes = err_msg,
            no = "Unknown. Reason not provided by server")
        message(paste0("Interview ", interview_id, " not reassigned. ",
            "\nReason: ", err_msg))
    # if unexpected status code
    } else if (!status %in% c(200, 404, 406)) {
        success <- FALSE
        message(paste0("Interview ", interview_id, "not reassigned. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {  
        return(success)
    }

}

#' Reassign an interview to a supervisor
#'
#' Has the server-side side-effect of reassigning a single interview to a supervisor. For interactive use, prints a message about the outcome. For programmatic use, if `verbose = TRUE`, returns a logical value about whether interview successfully assigned, which may be useful for functionst that need to track success and failure of operations.
#' 
#' Wrapper for \code{PATCH ​/api​/v1​/interviews​/{id}​/assignsupervisor} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param user_id ID of target user. GUID from server.
#' @param user_name User name of target user.
#' @param verbose Logical. If `verbose = TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome.
#'
#' @import httr
#' @importFrom jsonlite toJSON
#'
#' @export
assign_interview_to_sup <- function(
    interview_id,
    user_id = "",
    user_name = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")


    # check that either user ID or user name provided
    if (user_id == "" & user_name == "") {
        stop("Either `user_id` or `user_name` must be provided.")
    }

    # if user_id provided, check that it is a valid guid
    if (user_id != "") {
        check_guid(
            guid = user_id, 
            fail_msg = "User ID in `user_id` is not a valid GUID.")
    }

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/assignsupervisor")

    # form the body for the assignment request
    # ... if only `user_id` provided
    if (user_id != "" & user_name == "") {
        body <- list(
            ResponsibleId = user_id
        )
    # ... if only `user_name` provided
    } else if (user_id == "" & user_name != "") {
        body <- list(
            ResponsibleName = user_name
        )
    # ... if both `user_id` and `user_name` provided
    } else if (user_id != "" & user_name != "") {
        body <- list(
            ResponsibleId = user_id,
            ResponsibleName = user_name
        )
    }

    # post request
    response <- httr::PATCH(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose, return success (TRUE/FALSE) and, if applicable and message.

    status <- httr::status_code(response)

    # if interview assigned
    if (status == 200) {
        success <- TRUE
        message(paste0("Interview ", interview_id, " successfully reassigned. "))            
    # if interview not found
    } else if (status == 404) {
        success <- FALSE
        message(paste0("Interview ", interview_id, " not reassigned. ",
            "\nReason: Interview not found."))
    # if interview cannot be reassigned
    } else if (status == 406) {
        success <- FALSE
        err_msg <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
        err_msg <- ifelse(
            test = !is.na(err_msg), 
            yes = err_msg,
            no = "Unknown. Reason not provided by server")
        message(paste0("Interview ", interview_id, " not reassigned. ",
            "\nReason: ", err_msg))
    # if unexpected status code
    } else if (!status %in% c(200, 404, 406)) {
        success <- FALSE
        message(paste0("Interview ", interview_id, "not reassigned. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {    
        return(success)
    }

}


#' Approve an interview as headquarters
#'
#' Has server-side side-effect of approving a single interview as headquarters. If `verbose = TRUE`, return logical outcome and print message.
#' 
#' Wrapper for \code{PATCH /api/v1/interviews/{id}/hqapprove} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param comment Comment to post upon approval.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome.
#'
#' @import httr
#'
#' @export
approve_interview_as_hq <- function(
    interview_id,
    comment = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/hqapprove")

    # form query portion of request
    query <- list(
        id = interview_id,
        comment = comment
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # get assignments from the server
    response <- httr::PATCH(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message

    status <- httr::status_code(response)

    # approved
    if (status == 200) {
        result <- TRUE
        message(paste0("Interview ", interview_id, " successfully approved. "))            
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not approved. ",
            "\nReason: Interview not found."))
    # cannot be approved because of status
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not approved. ",
            "\nReason: Interview not in status to be approved."))
    # unknown outcome
    } else if (!status %in% c(200, 404, 406)) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not approved. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)    
    }

}

#' Reject an interview as headquarters
#'
#' Has server-side side-effect of rejecting a single interview as Headquarters. If `verbose = TRUE`, return logical outcome and print message.
#' 
#' Wrapper for \code{PATCH /api/v1/interviews/{id}/hqreject} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data.
#' @param comment Comment to post upon rejection.
#' @param responsible_id User ID. GUID from server or \code{interview__id} from exported data. User ID of the interviewer/supervisor to which the interview should be rejected.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome. 
#'
#' @import httr
#'
#' @export
reject_interview_as_hq <- function(
    interview_id,
    comment = "",
    responsible_id = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # if responsible ID provided, check that valid GUID
    if (responsible_id != "") {
        check_guid(
            guid = responsible_id, 
            fail_msg = "User ID in `responsible_id` is not a valid GUID.")
    }

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/hqreject")

    # form query portion of request
    query <- list(
        id = interview_id,
        comment = comment,
        responsibleId = responsible_id
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # get assignments from the server
    response <- httr::PATCH(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message

    status <- httr::status_code(response)

    # approved
    if (status == 200) {
        result <- TRUE
        message(paste0("Interview ", interview_id, " successfully rejected. "))            
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not rejected. ",
            "\nReason: Interview not found."))
    # cannot be approved because of status
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not rejected. ",
            "\nReason: Interview not in status to be rejected."))
    # unknown outcome
    } else if (!status %in% c(200, 404, 406)) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not rejected. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)    
    }

}


#' Unapprove an interview
#'
#' Has server-side side-effect of unapproving a single interview, returningn it to Approved by Headquarters status. For interactive use, prints a message to describe the outcome of the action. If \code{verbose = TRUE}, returns a logical value about the outcome that may be useful for functions that need to track successes and failures. 
#' 
#' Wrapper for \code{PATCH /api/v1/interviews/{id}/hqunapprove} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param comment Comment to post upon unapproval
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome.
#'
#' @import httr
#'
#' @export
unapprove_interview <- function(
    interview_id,
    comment = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # qnr_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/hqunapprove")

    # form query portion of request
    query <- list(
        id = interview_id,
        comment = comment
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # get assignments from the server
    response <- httr::PATCH(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message

    status <- httr::status_code(response)

    # approved
    if (status == 200) {
        result <- TRUE
        message(paste0("Interview ", interview_id, " successfully unapproved. "))            
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not unapproved. ",
            "\nReason: Interview not found."))
    # cannot be approved because of status
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not unapproved. ",
            "\nReason: Interview not in status to be unapproved."))
    # unknown outcome
    } else if (!status %in% c(200, 404, 406)) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not unapproved. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)    
    }

}

#' Reject an interview as supervisor
#'
#' Has server-side side-effect of rejecting a single interview as a supervisor. For interactive use, prints a message to describe the outcome of the action. If \code{verbose = TRUE}, returns a logical value about the outcome that may be useful for functions that need to track successes and failures.
#' 
#' Wrapper for \code{PATCH /api/v1/interviews/{id}/reject} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param comment Comment to post upon rejection
#' @param responsible_id User to receive rejected interview. User ID. GUID from server.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome. 
#'
#' @import httr
#'
#' @export
reject_interview_as_sup <- function(
    interview_id,
    comment = "",
    responsible_id = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # if responsible ID provided, check that valid GUID
    if (responsible_id != "") {
        check_guid(
            guid = responsible_id, 
            fail_msg = "User ID in `responsible_id` is not a valid GUID.")
    }

    # form the base URL
    base_url <- paste0(server, "/api/v1/interviews/", interview_id, "/reject")

    # form query portion of request
    query <- list(
        id = interview_id,
        comment = comment,
        responsibleId = responsible_id
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # get assignments from the server
    response <- PATCH(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message

    status <- httr::status_code(response)

    # rejected
    if (status == 200) {
        result <- TRUE
        message(paste0("Interview ", interview_id, " successfully rejected. "))
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not rejected. ",
            "\nReason: Interview not found."))
    # cannot be rejected because of status
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not rejected. ",
            "\nReason: Interview not in status to be rejected."))
    # unknown outcome
    } else if (!status %in% c(200, 404, 406)) {
        result <- FALSE
        message(paste0("Interview ", interview_id, " not rejected. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)
    
    }

}

# POST /api/v1/interviews/{id}/comment/{questionId}
# Leave a comment on a question
# NOTE: NOT WORTH DOING SINCE UNLIKELY USER WILL HAVE QUESTION ID

#' Leave a comment on a question
#'
#' Has server-side side-effect of posting a comment to a target question. To target a question, specify it in `variable_name`. If the question is in a roster, specify the row code as a comma-separated set of coordinates in `row_number`.
#' 
#' Wrapper for \code{POST /api/v1/interviews/{id}/comment-by-variable/{variable}} endpoint
#'
#' @param interview_id Interview ID. GUID from server or \code{interview__id} from exported data
#' @param variable_name Variable name. User name from Designer.
#' @param row_number Row number. Use roster-specific row code.
#' @param comment Comment to post.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome. 
#'
#' @import httr
#'
#' @export
comment_question <- function(
    interview_id,
    variable_name,
    row_number = "",
    comment,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs:
    # interview_id
    check_guid(
        guid = interview_id, 
        fail_msg = "Interview ID in `interview_id` is not a valid GUID.")

    # formulate API call
    base_url <- paste0(server,
        "/api/v1/interviews/", interview_id, 			# interview
        "/comment-by-variable/", variable_name 		# variable
    )

    # form query portion of request
    query = list(
        rosterVector = row_number,
        comment = comment
    )

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # make request
    response <- httr::POST(
        url = url,
        authenticate(user = user, password = password),
        accept_json(),
        content_type_json()
    )

    # return result
    # if verbose mode, return result and print message

    status <- httr::status_code(response)

    # commented
    if (status == 200) {
        result <- TRUE
        message(paste0("Question ", variable_name, " successfully commented. "))            
    # unknown outcome
    } else {
        result <- FALSE
        message(paste0("Question ", interview_id, " not commented. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)
    }

}
