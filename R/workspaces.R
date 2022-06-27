# GET ​/api​/v1​/workspaces
# List existing workspaces

#' @noRd 
get_workspaces_count <- function(
    start = 0,
    length = 1,
    user_id = "",
    include_disabled = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

# form the base URL
base_url <- paste0(server, "/api/v1/workspaces")

# compose query
# match function params to expected query params
query <- list(
    Start = start,
    Length = length,
    UserId = user_id,
    IncludeDisabled = logical_to_string(include_disabled)
)
# remove those note specified
query <- query[query != ""]

# compose the full URL: base + query parameters
url <- httr::modify_url(
    url = base_url,
    query = query
)

# get workspaces
response <- httr::GET(
    url = url,
    httr::authenticate(user = user, password),
    httr::accept_json(),
    httr::content_type_json()
)

result <- httr::status_code(response)

if (result == 200) {

    # return count of workspaces
    workspace_count <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$TotalCount

    return(workspace_count)

} else if (result != 200) {
    warning("No workspaces found.")
}

}

#' @noRd 
get_workspaces_batch <- function(
    start = "",
    length = 20,
    user_id = "",
    include_disabled = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces")

    # compose query
    # match function params to expected query params
    query <- list(
        Start = start,
        Length = length,
        UserId = user_id,
        IncludeDisabled = logical_to_string(include_disabled)
    )
    # remove those note specified
    query <- query[query != ""]

    # compose the full URL: base + query parameters
    url <- httr::modify_url(
        url = base_url,
        query = query
    )

    # get workspaces
    response <- httr::GET(
        url = url,
        httr::authenticate(user = user, password),
        httr::accept_json(),
        httr::content_type_json()
    )

    # return workspaces
    df <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$Workspaces
    return(df)

}

#' Get list of workspaces
#' 
#' Get list of workspaces that meet query criteria. The list is affected by what the user can "see". If the credentials are admin credentials, all workspaces will be returned. If the user is an API user, then only those workspaces where the API user is present, will be included.
#' 
#' Wrapper for the `GET ​/api​/v1​/workspaces` endpoint.
#' 
#' @param user_id GUID. Searches for workspaces where user with user ID is assigned.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin or API user name
#' @param password Admin or API user password
#' @param include_disabled Logical. Determine whether list of workspaces should include those that have been disabled or not.
#' 
#' @return Data frame of workspaces. Contains columns: `Name`, the name ID; `DisplayName`, the name in the GUI; and `DisabledAtUtc`, when the workspace was disabled or NA if not disabled.
#' 
#' @importFrom assertthat assert_that
#' @importFrom rlang abort
#' @importFrom purrr map_dfr
#' 
#' @export 
get_workspaces <- function(
    user_id = "",
    include_disabled = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password        
) {

    # check inputs
    # interview_id is empty or a GUID
    assertthat::assert_that(
        (user_id == "" | is_guid(user_id)),
        msg = "User ID in `user_id` is not a valid GUID."
    )

    # include_disabled
    if (!include_disabled %in% c(TRUE, FALSE)) {
        assertthat::assert_that(
            assertthat::is.flag(verbose),
            msg = "`include_disabled` must be `TRUE` or `FALSE`."
        )
    }

    # get total count of workspaces
    tryCatch(
        warning = function(cnd) rlang::abort(conditionMessage(cnd)),
        total_count <- get_workspaces_count(
            user_id = user_id,
            include_disabled = include_disabled,
            server = server,
            user = user,
            password = password
        )
    )

    # return all workspaces as a data frame
    df <- purrr::map_dfr(
        .x = seq(from = 0, to = total_count, by = 20),
        .f = get_workspaces_batch,
            length = 20,
            user_id = user_id,
            include_disabled = include_disabled,
            server = server,
            user = user,
            password = password
        )
    return(df)

}

# POST ​/api​/v1​/workspaces
# Creates new workspace. Accessible only to administrator

#' Create a workspace
#' 
#' Create a workspace with name ID `name` and name displayed in GUI `display_name`.
#' 
#' Note: this function requires the credentials of an admin user.
#' 
#' Wrapper for `POST ​/api​/v1​/workspaces` endpoint.
#' 
#' @param name Character. Name identifier of workspace.
#' @param display_name Character. Name displayed in GUI to describe workspace.
#' @param verbose Logical. If `TRUE`, return a Boolean value about whether operation succeeded.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin user name
#' @param password Admin password
#' 
#' @return Server-side side-effect of creating a workspace.
#' 
#' @importFrom assertthat assert_that is.flag
#' @import httr
#' @importFrom jsonlite toJSON
#' 
#' @export 
create_workspace <- function(
    name,
    display_name,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs
    # TODO: check inputs
    # TODO: devise a way to check that credentials are admin credentials

    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )

    # display_name
    assertthat::assert_that(
        is_workspace_display_name(display_name),
        msg = paste0(
            "Workspace display name is not valid.\n",
            "Must be 300 characters or less."
        )
    )

    # `verbose` is Boolean
    assert_that(
        is.flag(verbose),
        msg = "The value of `verbose` should be Boolean: `TRUE` or `FALSE`."
    )

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces")

    # compose body of post
    # match function params to expected keys in body
    body <- list(
        Name = name,
        DisplayName = display_name
    )

    # post request for new workspace
    response <- httr::POST(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()        
    )

    status <- httr::status_code(response)

    # workspace created
    if (status == 201) {
        result <- TRUE
        message(paste0("Workspace ", name, " successfully created"))
    # workspace not created
    } else if (status == 400) {
        result <- FALSE
        message(paste0(
            "Workspace ", name, " not created.\n",
            "Reason: Bad request. HTTP code 400.\n",
            "Please revise `name` and/or `display_name`."
        ))
    # unknown outcome
    } else if (!status %in% c(200, 400)) {
        result <- FALSE
        message(paste0(
            "Workspace ", name, " not created.\n",
            "Reason: Unknown. HTTP code", status, "."
        ))
    }

    # if verbose, return TRUE/FALSE if operation succeeded/failed
    if (verbose == TRUE) {
        return(result)
    }

}

# GET ​/api​/v1​/workspaces​/{name}
# Get single workspace details

#' Get details of a single workspace
#' 
#' Rather than get the details for all workspaces, get them for a single workspace.
#' 
#' Wrapper for `GET ​/api​/v1​/workspaces​/{name}` endpoint
#' 
#' @param name Character. Name identifier of workspace.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin user name
#' @param password Admin password
#' 
#' @return Data frame containing workspace details: `Name`, the name ID; `DisplayName`, the name displayed in the GUI; and `DisabledAtUtc`, the time the workspace was disabled or `NA` if not disabled
#' 
#' @import httr
#' 
#' @export 
get_workspace_details <- function(
    name,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # check inputs
    # TODO: check that user is admin

    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )

    # form the URL
    url <- paste0(server, "/api/v1/workspaces/", name)

    # get workspace details from the server
    response <- httr::GET(
        url = url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    if (status == 200) {
        # extract values from JSON response
        vals <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)
        # replace any NULL values with NA
        vals <- lapply(vals, FUN = function(x) ifelse(is.null(x), NA, x))
        # convert from list to data frame
        df <- as.data.frame(vals)
        # return as data frame
        return(df)
    } else if (status == 404) {
        message(paste0(
            "Workspace ", name, " could not be found. HTTP code 404.\n",
            "Please check that the name provided in `name` is correct."
        ))
    } else if (!status %in% c(200, 404)) {
        message(paste0(
            "Workspace ", name, " could not be found.\n",
            "Reason: Unknown error. HTTP code", status, ".",
        ))
    }

}

# PATCH ​/api​/v1​/workspaces​/{name}
# Updates workspace

#' Update workspace
#' 
#' Updates workspace attributes. For the moment, this function/endpoint only updates the display name.
#' 
#' Wrapper for `PATCH ​/api​/v1​/workspaces​/{name}` endpoint
#' 
#' @param name Character. Name identifier of workspace to update.
#' @param display_name Character. Name displayed in GUI to describe workspace. This value is updated.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin or API user name
#' @param password Admin or API password
#' 
#' @return Server-side side-effect of updating workspace
#' 
#' @import httr
#' 
#' @export 
update_workspace <- function(
    name,
    display_name,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # check inputs
    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )

    # display_name
    assertthat::assert_that(
        is_workspace_display_name(display_name),
        msg = paste0(
            "Workspace display name is not valid.\n",
            "Must be 300 characters or less."
        )
    )

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces/", name)

    # compose body of post
    # match function params to expected keys in body
    body <- list(
        DisplayName = display_name
    )
    # remove those not specified
    # TODO: disable, in future, where multiple values can be updated
    # body <- body[body != ""]

    # patch archive status on server
    response <- httr::PATCH(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # workspace updated
    if (status == 204) {
        message(paste0("Workspace ", name, " successfully updated"))
    # validation failed
    } else if (status == 400) {
        message(paste0(
            "Workspace ", name, " not updated.\n",
            "Reason: validation failed.\n",
            "HTTP code: 400.\n",
            "Please supply a valid value for `name`."
        ))
    # user not authorized to make changes to workspace
    } else if (status == 403) {
        message(paste0(
            "Workspace ", name, " not updated.\n",
            "Reason: user not authorized otmake changes to workspace.\n",
            "HTTP code: 403.\n",
            "Please set credentials for a user with adequate privileges: either an admin user or an API user associated with this workspace."            
        ))
    # workspace not found
    } else if (status == 404) {
        message(paste0(
            "Workspace ", name, " not updated.\n",
            "Reason: workspace not found.\n",
            "HTTP code: 404.\n",
            "Please check that `name` is valid."
        ))
    # unknown error
    } else if (!status %in% c(204, 400, 403, 404)) {
        message(paste0(
            "Workspace ", name, " not updated.\n",
            "Reason: unknown error.\n",
            "HTTP code: ", status, "."
        ))
    }

}

# DELETE ​/api​/v1​/workspaces​/{name}
# Delete workspace

#' Delete a workspace
#' 
#' Deletes workspace whose name matches `name`.
#' 
#' Wrapper for `DELETE ​/api​/v1​/workspaces​/{name}` endpoint.
#' 
#' @param name Character. Name identifier of workspace to delete.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin or API user name
#' @param password Admin or API password
#' 
#' @return Server-side side-effect of deleting a workspace.
#' 
#' @import httr
#' 
#' @export 
delete_workspace <- function(
    name,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

    # check inputs
    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces/", name)

    # request workspace be deleted
    response <- httr::DELETE(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # successful
    if (status == 200) {

        message(paste0("Workspace ", name, " successfully deleted."))

    # not successful for unknown reason
    } else if (status != 200) {

        message(paste0(
            "Workspace ", name, " not deleted.\n",
            "Reason: unknown error. HTTP code: ", status, "."
        ))

    }


}

# GET ​/api​/v1​/workspaces​/status​/{name}
# Request server for information about workspace status and it's ability to delete

#' Get detailed workspace status
#' 
#' Obtains a data frame with details about the workspace status and contents
#' 
#' Wrapper for `GET ​/api​/v1​/workspaces​/status​/{name}` endpoint.
#' 
#' @param name Character. Name identifier of workspace to delete.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin or API user name
#' @param password Admin or API password
#' 
#' @return Data frame. Contains columns: `CanBeDeleted`, `WorkspaceName`, `WorkspaceDisplayName`, `ExistingQuestionnairesCount`, `InterviewersCount`, `SupervisorsCount`, `MapsCount`
#' 
#' @import httr
#' 
#' @export 
get_workspace_status <- function(
    name,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password   
) {

    # check inputs
    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces/status/", name)

    # request workspace status
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # successful
    if (status == 200) {

        df <- as.data.frame(jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE))
        return(df)

    # not successful for unknown reason
    } else if (status != 200) {

        message(paste0(
            "Workspace status for ", name, " not found.\n",
            "Reason: unknown error. HTTP code: ", status, "."
        ))

    }

}

# POST ​/api​/v1​/workspaces​/{name}​/disable
# Disables specified workspace

#' Disable a workspace
#' 
#' Disable the workspace with name ID `name`.
#' 
#' Wrapper for `POST ​/api​/v1​/workspaces​/{name}​/disable` endpoint.
#' 
#' @param name Character. Name identifier of workspace to disable.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin user name
#' @param password Admin password
#' 
#' @return Server-side side-effect of disabling a workspace.
#' 
#' @import httr
#' 
#' @export 
disable_workspace <- function(
    name,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password  
) {

    # check inputs
    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )  

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces/", name, "/disable")

    # request workspace be disabled
    response <- httr::POST(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # workspace disabled
    if (status == 204) {
        message(paste0("Workspace ", name, " successfully disabled."))
    # validation failed
    } else if (status == 400) {
        message(paste0(
            "Workspace ", name, " not disabled.\n",
            "Reason: validation failed. HTTP code: 400."
        ))
    # workspace not found
    } else if (status == 404) {
        message(paste0(
            "Workspace ", name, " not disabled.\n",
            "Reason: workspace not found. HTTP code: 404.\n",
            "Please check the name specified in `name`."
        ))
    # unknown error
    } else if (!status %in% c(204, 400, 404)) {
        message(paste0(
            "Workspace ", name, " not disabled.\n",
            "Reason: workspace not found. HTTP code: ", status, "."
        ))
    }

}

# POST ​/api​/v1​/workspaces​/{name}​/enable
# Enables specified workspace

#' Enable a workspace
#' 
#' Enable a previously disabled workspace with name `name`.
#' 
#' Wrapper for the `POST ​/api​/v1​/workspaces​/{name}​/enable` endpoint.
#' 
#' @param name Character. Name identifier of workspace to enable.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin user name
#' @param password Admin password
#' 
#' @import httr
#' 
#' @export 
enable_workspace <- function(
    name,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password      
) {

    # check inputs
    # name
    assertthat::assert_that(
        is_workspace_name(name),
        msg = paste0(
            "Workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )   

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces/", name, "/enable")

    # request workspace be disabled
    response <- httr::POST(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # workspace disabled
    if (status == 204) {
        message(paste0("Workspace ", name, " successfully enabled."))
    # validation failed
    } else if (status == 400) {
        message(paste0(
            "Workspace ", name, " not enabled.\n",
            "Reason: validation failed. HTTP code: 400."
        ))
    # workspace not found
    } else if (status == 404) {
        message(paste0(
            "Workspace ", name, " not enabled.\n",
            "Reason: workspace not found. HTTP code: 404.\n",
            "Please check the name specified in `name`."
        ))
    # unknown error
    } else if (!status %in% c(204, 400, 404)) {
        message(paste0(
            "Workspace ", name, " not enabled.\n",
            "Reason: workspace not found. HTTP code: ", status, "."
        ))
    }

}

# POST ​/api​/v1​/workspaces​/assign
# Assigns workspaces to user.

#' Manager user assignment to a workspace
#' 
#' Manage which users are present in which workspaces. For now, only headquarters and API users may be managed. Actions available: assign, add, and remove. The assign action moves a user to the target workspace (exclusively). The add action adds the user to the workspace while keeping them in previously assigned workspaces. The remove action removes them from the target workspace.
#' 
#' Wrapper for `POST ​/api​/v1​/workspaces​/assign` endpoint.
#' 
#' @param users Character vector. GUID for user.
#' @param workspaces Character vector. Name ID of workspace
#' @param action Character. One of the following: Assign, Add, Remove.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user Admin user name
#' @param password Admin password
#' 
#' @import httr
#' 
#' @export 
assign_users_to_workspaces <- function(
    users,
    workspaces,
    action = "Assign",
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password       
) {

    # check inputs
    # users is a character vector
    assertthat::assert_that(
        (typeof(users) == "character" & rlang::is_vector(users)),
        msg = "`users` should be a character vector"
    )

    # each element of users is a GUID
    assertthat::assert_that(
        all(is_guid(users)),
        msg = "All elements of `users` must be a valid GUID."
    )

    # workspaces is a character vector
    assertthat::assert_that(
        (typeof(workspaces) == "character" & rlang::is_vector(workspaces)),
        msg = "`workspaces` should be a character vector"
    )

    # workpaces names are valid names
    assertthat::assert_that(
        all(is_workspace_name(workspaces)),
        msg = paste0(
            "At least one workspace name is not valid. Names must be:\n",
            "- No more than 12 character.\n",
            "- Composed of lower-case letters and numbers."
        )
    )

    # action in c("Assign", "Add", "Remove")
    assertthat::assert_that(
        action %in% c("Assign", "Add", "Remove"),
        msg = "`action` should be one of the following: 'Assign', 'Add', 'Remove'"
    )

    # form the base URL
    base_url <- paste0(server, "/api/v1/workspaces/assign")

    # compose body of request
    # transform paramters into list of values with escaped double quotes
    user_array <- paste0('\"', users, '\"', collapse = ",")
    workspaces_array <- paste0('\"', workspaces, '\"', collapse = ",")
    # create JSON body by inserting parameters into their positions
    body <- paste0(
        '{',
            '\"UserIds\":[', user_array ,'],',
            '\"Workspaces\":[', workspaces_array, '],',
            '\"Mode\":\"', action, '\"',
        '}'        
    )

    # request workspace be disabled
    response <- httr::POST(
        url = base_url,
        body = body,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    status <- httr::status_code(response)

    # success
    if (status == 204) {
        message(paste0("Action successful."))
    # failed validation
    } else if (status == 400) {
        message(paste0(
            "Action failed.\n",
            "Reason: failed validation. HTTP code: 400."
        ))
    # unknown error
    } else if (!status %in% c(204, 400)) {
        message(paste0(
            "Action failed.\n",
            "Reason: failed validation. HTTP code: ", status, "."
        ))
    }

}
