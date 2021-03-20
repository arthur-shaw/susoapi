# GET /api/v1/interviewers/{id}
# Gets detailed info about single interviewer
# NOTE: will develop more general /api/v1/users/{id} instead

# GET /api/v1/interviewers/{id}/actions-log
# Returns audit log records for interviewer. You can specify "start" and "end" parameters in query string to get range results.


#' Get action log for single user
#'
#' Fetches the log of user actions on the tablet. This includes creating, opening, editing, closing, and sending interviews. Returns the log as a data frame.
#' 
#' Wrapper for \code{GET /api/v1/interviewers/{id}/actions-log} endpoint
#'
#' @param user_id User ID. GUID from server.
#' @param start Character. Date in "YYYY-MM-DD" format
#' @param end Character. Date in "YYYY-MM-DD" format
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame containing the action log.
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @export 
get_user_action_log <- function(
    user_id, 
    start = "",
    end = "",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password        
) {

    # check inputs

    # user_id
    check_guid(
        guid = user_id, 
        fail_msg = "User ID in `user_id` is not a valid GUID.")

    # start
    # datetime
    # TODO: add check on input format

    # end
    # datetime
    # TODO: add check on input format

    # form base URL
    base_url <- paste0(server, "/api/v1/interviewers/", user_id, "/actions-log")

    # form the query terms
    query <- list(
        start = start,
        end = end
    )

    # compose the full URL: base + query paramters
    url <- httr::modify_url(
        url = base_url,
        query = query)

    # send request
    response <- httr::GET(
        url = url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # parse response
    status <- httr::status_code(response)
    # if request successful...
    if (status == 200) {

        message(paste0("Action log successfully obtained for user ", user_id, "."))

        # extract the action log payload
        action_log <- fromJSON(content(response, as = "text"), flatten = TRUE)

        # if log entries in payload
        n_action_log_entries <- length(action_log)
        if (n_action_log_entries > 0) {

            action_log$UserId <- user_id

        # if no log entries in payload
        } else if (n_action_log_entries == 0) {

            # create empty data frame with expected columns
            action_log <- data.frame(
				Time = NA_character_,
				Message = NA_character_, 
				UserId = as.character(user_id),
				stringsAsFactors = FALSE
			)

        }

        message("Log contains ", nrow(action_log), " entries for specified dates.")
        return(action_log)

    # if request unsuccessful, display failure
    } else {

        message(paste0("Action log not obtained. HTTP code: ", status))

    }


}

#' Count supervisors
#'
#' Provides count for iterative request for supervisors. Wrapper for \code{GET /api/v1/supervisors} endpoint
#'
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Count of supervisors
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @noRd
user_get_sups_count <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/supervisors/")

    # form the body for the request
    query <- list(
        limit = 1,
        offset = 1
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

    # return total count
    total_count <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$TotalCount
    return(total_count)

}

#' Get list of some supervisors
#'
#' Get list of N supervisors. Wrapper for \code{GET /api/v1/supervisors} endpoint
#'
#' @param limit
#' @param offset
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame that contains supervisors and their attributes.
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @noRd
user_get_list_sups <- function(
    limit = 40,
    offset = 1,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/supervisors/")

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

    # return supervisor list
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Users
    return(df)

}

#' Get list of all supervisors
#'
#' Get all supervisors and their attributes. Wrapper for \code{GET /api/v1/supervisors} endpoint
#'
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame that contains supervisors and their attributes.
#'
#' @importFrom purrr map_dfr
#'
#' @noRd 
user_get_list_sups_all <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # get total count of assignments
    total_count <- user_get_sups_count(
        server = server,
        user = user,
        password = password)

    # return all assignments as a dataframe
    df <- purrr::map_dfr(
        .x = seq(
            from = 1,
            to = ceiling(total_count/40),
            by = 1),
        .f = user_get_list_sups,
        server = server,
        limit = 40,
        user = user,
        password = password)

    return(df)

}

# GET /api/v1/supervisors/{id}
# Gets detailed info about single user

# NOTE: will develop more general /api/v1/users/{id} instead


#' Count interviewers for supervisor
#'
#' Wrapper for \code{GET /api/v1/supervisors/{supervisorId}/interviewers} endpoint
#'
#' @param sup_id Supervisor's user ID. GUID from server.
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
user_get_ints_count <-  function(
    sup_id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs:
    # sup_id
    check_guid(
        guid = sup_id, 
        fail_msg = "Supervisor ID in `sup_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/supervisors/", sup_id, "/interviewers")

    # form the body for the request
    query <- list(
        limit = 1,
        offset = 1
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

    # return total count
    total_count <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$TotalCount
    return(total_count)

}

#' Get list of some interviewers for supervisor
#'
#' Wrapper for \code{GET /api/v1/supervisors/{supervisorId}/interviewers} endpoint
#'
#' @param sup_id Supervisor's user ID. GUID from server.
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
user_get_list_ints <- function(
    sup_id,
    limit = 40,
    offset = 1,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs:
    # sup_id
    check_guid(
        guid = sup_id, 
        fail_msg = "Supervisor ID in `sup_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/supervisors/", sup_id, "/interviewers")

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

    # return interviewers
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Users
    if (length(df) == 0) {
        df <- data.frame(
            IsLocked = NA,
            CreationDate = NA,
            Email = NA,
            DeviceId = NA,
            UserId = NA,
            UserName = NA,
            SupervisorId = sup_id
	    )
    } else {
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        df$SupervisorId <- sup_id
    }

    return(df)

}

#' Get list all interviewers for supervisor
#'
#' Wrapper for \code{GET /api/v1/supervisors/{supervisorId}/interviewers} endpoint
#'
#' @param sup_id Supervisor user ID. GUID from server.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of interviewers and their attributes.
#'
#' @importFrom purrr map_dfr
#' 
#' @noRd 
user_get_list_ints_all <- function(
    sup_id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # check inputs:
    # sup_id
    check_guid(
        guid = sup_id, 
        fail_msg = "Supervisor ID in `sup_id` is not a valid GUID.")

    # get total count of interviewers for supervisor
    total_count <- user_get_ints_count(
        sup_id = sup_id,
        server = server,
        user = user,
        password = password)

    # return all interviewers as a dataframe
    if (total_count > 0) {
        df <- purrr::map_dfr(
            .x = seq(
                from = 1,
                to = ceiling(total_count/40),
                by = 1),
            .f = user_get_list_ints,
            sup_id = sup_id,
            server = server,
            limit = 40,
            user = user,
            password = password)
    } else {
        df <- data.frame(
            IsLocked = NA,
            CreationDate = NA,
            Email = NA,
            DeviceId = NA,
            UserId = NA,
            UserName = NA,
            SupervisorId = sup_id
	    )
    }

    return(df)

}

#' Gets detailed info about single user
#'
#' Wrapper for \code{GET /api/v1/users/{id}} endpoint
#'
#' @param user_id User ID, user name, or email.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of single user's details.
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @export
get_user_details <- function(
    user_id,   # accepts: UserId, UserName, or Email
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/users/", user_id)

    # post request
    response <- httr::GET(
        url = base_url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # provide message according to outcome
    status <- httr::status_code(response)

    # rejected
    if (status == 200) {

        message(paste0("Successfully got details for user ", user_id, ". "))

        # return user details
        df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        return(df)

    # not found
    } else if (status == 404) {

        message(paste0("Failed to get details for user ", user_id, ". ",
            "\nReason: User not found. Check the GUID, user name, or email provided."))

    # unknown outcome
    } else if (!status %in% c(200, 404)) {

        message(paste0("Failed to get details for user ", user_id, ". ",
            "\nReason: Unknown. HTTP code: ", status, "."))

    }

}

#' Get list of all supervisors
#'
#' Fetch list of all supervisors and their attributes
#'
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of supervisors and their attributes.
#' 
#' @importFrom purrr map_dfr
#' @import dplyr
#'
#' @export
get_supervisors <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # get count of supervisors
    total_count <- user_get_sups_count(
        server = server,
        user = user,
        password = password)

    # get list all supervisors
    supervisor_list <- user_get_list_sups_all(
        server = server,
        user = user,
        password = password)

    # add DeviceId if it does not exist
    if(!"DeviceId" %in% names(supervisor_list)) {
        supervisor_list$DeviceId <- NA_character_
    }

    supervisor_list <- select(supervisor_list, .data$UserId, .data$DeviceId)

    # get details for all supervisors
    supervisor_details <- purrr::map_dfr(
        .x = supervisor_list$UserId,
        .f = get_user_details,
        server = server,
        user = user,
        password = password
    ) %>%
    # ... and replace white space values with NA
    dplyr::mutate_if(
        .predicate = is.character,
        .funs = ~ if_else(
            condition = . == "",
            true = NA_character_,
            false = .,
            missing = .)
        )

    # return supervisors with detailed attributes
    supervisors <- dplyr::left_join(supervisor_details, supervisor_list, by = "UserId")
    return(supervisors)

}

#' Get list of all interviewers
#'
#' Fetch list of all interviewers and their attributes, including their supervisor
#'
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of interviewers and their details.
#' 
#' @importFrom purrr map_dfr
#' @import dplyr
#'
#' @export
get_interviewers <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password    
) {

    # get supervisors
    sups_list <- user_get_list_sups_all(server = server, user = user, password = password)
    
    # get interviewers
    # get list
    ints_list <- purrr::map_dfr(
        .x = sups_list$UserId,
        .f = user_get_list_ints_all,
        server = server, user = user, password = password) %>%
        filter(!is.na(.data$UserId))

    # add column missing user never connected
    if (!"DeviceId" %in% names(ints_list)) {
        ints_list <- mutate(ints_list, DeviceId = NA_character_)
    }   

    # get details
    ints_detail <- purrr::map_dfr(
        .x = ints_list$UserId,
        .f = get_user_details,
        server = server,
        user = user,
        password = password
    ) %>%
    # ... and replace white space values with NA
    dplyr::mutate_if(
        .predicate = is.character,
        .funs = ~ if_else(
            condition = . == "",
            true = NA_character_,
            false = .,
            missing = .)
        )

    # combine list and detail attributes
    ints_device <- dplyr::select(ints_list, .data$UserId, .data$SupervisorId, .data$DeviceId)
    ints <- dplyr::left_join(ints_detail, ints_device, by = "UserId") %>%
    # add SupervisorName and DeviceId attributes
    dplyr::left_join(
        dplyr::select(sups_list,
            SupervisorId = .data$UserId,
            SupervisorName = .data$UserName),
        by = "SupervisorId")

    # return combination of list and details attributes
    return(ints)

}

#' Archives interviewer or supervisor with all his interviewers
#'
#' Wrapper for \code{PATCH /api/v1/users/{id}/archive} endpoint
#'
#' @param user_id User ID. GUID from server.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Side-effect on the server: APPROVE interview. If `verbose == TRUE`, return logical outcome and print message.
#' 
#' @export
#'
#' @import httr
#'
#' @examples
archive_user <- function(
    user_id,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

    # check inputs
    # user_id
    check_guid(
        guid = user_id, 
        fail_msg = "User ID in `user_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/users/", user_id, "/archive")

    # oatch archive status on server
    response <- httr::PATCH(
        url = base_url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message
    status <- httr::status_code(response)

    # archived
    if (status == 200) {
        result <- TRUE
        message(paste0("User ", user_id, " successfully archived. "))
    # user id cannot be parsed
    } else if (status == 400) {
        result <- FALSE
        message(paste0("User ", user_id, " not archived. ",
            "\nReason: User ID cannot be parsed."))
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("User ", user_id, " not archived. ",
            "\nReason: User with this ID not found."))
    # user does not exist
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", user_id, " not archived. ",
            "\nReason: User does not exist."))
    # unknown outcome
    } else if (!status %in% c(200, 400, 404, 406)) {
        result <- FALSE
        message(paste0("User ", user_id, " not archived. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)
    }

}

# PATCH /api/v1/users/{id}/unarchive
# Unarchives single user

#' Unarchives single user
#'
#' Wrapper for \code{PATCH /api/v1/users/{id}/unarchive} endpoint
#'
#' @param user_id User ID. GUID from server.
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Side-effect on the server: APPROVE interview. If `verbose == TRUE`, return logical outcome and print message.
#' 
#' @export
#'
#' @import httr
#'
#' @examples
unarchive_user <- function(
    user_id,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

    # check inputs
    # user_id
    check_guid(
        guid = user_id, 
        fail_msg = "User ID in `user_id` is not a valid GUID.")

    # form the base URL
    base_url <- paste0(server, "/api/v1/users/", user_id, "/unarchive")

    # patch archive status on server
    response <- httr::PATCH(
        url = base_url,
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message
    status <- httr::status_code(response)

    # unarchived
    if (status == 200) {
        result <- TRUE
        message(paste0("User ", user_id, " successfully unarchived. "))
    # user id cannot be parsed
    } else if (status == 400) {
        result <- FALSE
        message(paste0("User ", user_id, " not unarchived. ",
            "\nReason: User ID cannot be parsed."))
    # not found
    } else if (status == 404) {
        result <- FALSE
        message(paste0("User ", user_id, " not unarchived. ",
            "\nReason: User with this ID not found or does not exist."))
    # user does not exist
    } else if (status == 406) {
        result <- FALSE
        message(paste0("Interview ", user_id, " not unarchived. ",
            "\nReason: User with this ID is not an interviewer or supervisor."))
    } else if (status == 409) {
        result <- FALSE
        message(paste0("Interview ", user_id, " not unarchived. ",
            "\nReason: User cannot be unarchived."))
    # unknown outcome
    } else if (!status %in% c(200, 400, 404, 406, 409)) {
        result <- FALSE
        message(paste0("User ", user_id, " not archived. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)
    }

}

#' Creates new user with specified role.
#'
#' Creates a new uers with all the specified properties. Wrapper for \code{POST /api/v1/users} endpoint.
#'
#' @param role Character. Role in data collection. Accepted values: \code{"Supervisor"}, \code{"Interviewer"}
#' @param supervisor Character. User name of supervisor. Required for \code{role == "Interviewer"}. Omitted otherwise.
#' @param user_name Character. User name.
#' @param user_password Character. Password for user account.
#' @param full_name Character. Full name of user. (Optional)
#' @param phone_number Character. Phone number for user. (Optional)
#' @param email Character. Email address for user. (Optional)
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Side-effect on the server: APPROVE interview. If `verbose == TRUE`, return logical outcome and print message.
#' 
#' @export
#'
#' @importFrom assertthat assert_that
#' @import httr
#' @importFrom jsonlite toJSON fromJSON
#'
#' @examples
create_user <- function(
    role,
    supervisor = "",
    user_name,
    user_password = "",
    full_name = "",
    phone_number = "",
    email = "",
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs
    # role
    assertthat::assert_that(
        role %in% c("Supervisor", "Interviewer"),
        msg = "`role` must be either 'Supervisor' or 'Interviewer'."
    )

    # supervisor specified if role is interviewer
    if (role == "Interviewer") {
        assertthat::assert_that(
            supervisor != "",
            msg = "Must specify `supervisor` if `role = 'Interviewer'`"
        )
    }

    # form the base URL
    base_url <- paste0(server, "/api/v1/users/")

    # compose body of post
    # match function params to expected keys in body
    body <- list(
        Role = role,
        UserName = user_name,
        FullName = full_name,
        PhoneNumber = phone_number,
        Email = email,
        Password = user_password,
        Supervisor = supervisor
    )
    # remove those not specified
    body <- body[body != ""]

    # patch archive status on server
    response <- httr::POST(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        authenticate(user = user, password = password),
		accept_json(),
		content_type_json()
    )

    # if verbose mode, return result and print message
    status <- httr::status_code(response)

    # user created
    if (status == 200) {
        result <- TRUE
        user_id <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$UserId
        message(
            paste0(
                "User ", user_name, " successfully created.\n",
                "User ID: ", user_id
            ))
    # user cannot be created
    } else if (status == 400) {
        result <- FALSE
        errors <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Errors
        message(
            paste0(
                "User ", user_name, " not created.\n",
                "Error(s) encountered: \n",
                paste0(errors, collapse = "\n")
            ))
    # unknown outcome
    } else if (!status %in% c(200, 400)) {
        result <- FALSE
        message(paste0("User ", user_name, " not archived. ",
            "\nReason: Unknown. HTTP code: ", status, "."))
    }

    if (verbose == TRUE) {
        return(result)
    }

}
