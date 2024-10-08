# GET ​/api​/v1​/settings​/globalnotice
# Get global notice for the headquarters application

#' Get the global notice
#' 
#' Return the global notice, if any, set for workspaces within the user's scope. For admin, this is all workspaces. For an API user, this is all workspaces to which the user has been added.
#' 
#' Wrapper for the `GET ​/api​/v1​/settings​/globalnotice` endpoint.
#' 
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose global notice to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. User name
#' @param password Character. Password
#' 
#' @return Character. Global notice.
#' 
#' @import httr
#' @importFrom jsonlite fromJSON
#' 
#' @export 
get_global_notice <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password        
) {

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(
        server = server,
        workspace = workspace,
        user = user,
        password = password
    )

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v1/settings/globalnotice")
    )

    # get global notice
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()        
    )

    status <- httr::status_code(response)

    # success
    if (status == 200) {
        message("Successfully got the global notice")
        global_notice <- jsonlite::fromJSON(content(response, as = "text"))$Message
        return(global_notice)
    } else if (status != 200 ) {
    # unknown outcome
        message(paste0(
            "Unable to get the global notice.",
            "Reason: Unknown. HTTP code", status, "."
        ))
    }

}

# PUT /api​/v1​/settings​/globalnotice
# Set global notice for the headquarters application

#' Set the global notice
#' 
#' Set the global notice for workspaces within the user's scope. For admin, this is all workspaces. For an API user, this is all workspaces to which the user has been added.
#' 
#' Wrapper for the `PUT /api​/v1​/settings​/globalnotice` endpoint.
#' 
#' @param text Character. Text of the global notice to display.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose global notice to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. User name
#' @param password Character. Password
#' 
#' @return Server-side side-effect of setting the global notice text.
#' 
#' @import httr
#' 
#' @export 
set_global_notice <- function(
    text,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password        
) {

    # check inputs

    # text
    assertthat::assert_that(
        typeof(text) == "character",
        msg = "`text` must be a single character string."
    )

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(
        server = server,
        workspace = workspace,
        user = user,
        password = password
    )

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v1/settings/globalnotice")
    )

    # construct body
    body <- list(
        Message = text
    )

    # get global notice
    response <- httr::PUT(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()        
    )

    status <- httr::status_code(response)

    # success
    if (status == 204) {
        message("Successfully set the global notice")
    # bad request
    } else if (status == 400) {
        message(paste0(
            "Unable to set the global notice.\n",
            "Reason: Bad request. HTTP code: 400."
        ))
    # unknown outcome
    } else if (!status %in% c(204, 400)) {
        message(paste0(
            "Unable to set the global notice.",
            "Reason: Unknown. HTTP code", status, "."
        ))
    }

}

# DELETE ​/api​/v1​/settings​/globalnotice
# Remove global notice for the headquarters application

#' Delete the global notice
#' 
#' Delete the global notice for workspaces in the user's scope. For admin, this is all workspaces. For an API user, this is all workspaces to which the user has been added.
#' 
#' Wrapper for the `DELETE ​/api​/v1​/settings​/globalnotice` endpoint
#' 
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose global notice to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. User name
#' @param password Character. Password
#' 
#' @return Server-side side-effect of deleting the global notice text.
#' 
#' @import httr
#' 
#' @export 
delete_global_notice <- function(
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password   
) {

    # workspace:
    # - invalid name
    # - workspace does not exist
    check_workspace_param(
        server = server,
        workspace = workspace,
        user = user,
        password = password
    )

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v1/settings/globalnotice")
    )

    # get global notice
    response <- httr::DELETE(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()        
    )

    status <- httr::status_code(response)

    # success
    if (status == 204) {
        message("Successfully deleted the global notice")
    # unknown outcome
    } else if (status != 204 ) {
        message(paste0(
            "Unable to delete the global notice.",
            "Reason: Unknown. HTTP code", status, "."
        ))
    }

}


