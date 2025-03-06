
#' Check internet connection
#'
#' @return Logical
#'
#' @importFrom curl has_internet
#'
#' @noRd
check_internet <- function() {
    if (curl::has_internet()) {
        stop("No internet. Please check your connection and try again.")
    }
}


#' Check that GUID/UUID is valid
#'
#' Returns logical value after checking input against canonical form of GUID/UUID.
#'
#' @param guid GUID value. Character.
#' @param has_hyphens Logical. Determines pattern used for evalauting whether GUID is of valid form.
#'
#' @return Logical.
#'
#' @noRd
is_guid <- function(
    guid,
    has_hyphens = FALSE
) {

    # use a regex pattern to check if the input is of the proper form
    # if input contains hyphens, use this pattern
    if (has_hyphens) {
        grepl(x = guid, pattern = "[0-9a-f]{8}-[0-9a-f]{4}-[0-5][0-9a-f]{3}-[089ab][0-9a-f]{3}-[0-9a-f]{12}")
    } else {
    # otherwise, pattern must
        # have hyphens
        grepl(x = guid, pattern = "[0-9a-f]{8}-[0-9a-f]{4}-[0-5][0-9a-f]{3}-[089ab][0-9a-f]{3}-[0-9a-f]{12}") |
        # or not
        grepl(x = guid, pattern = "[0-9a-f]{8}[0-9a-f]{4}[0-5][0-9a-f]{3}[089ab][0-9a-f]{3}[0-9a-f]{12}")
    }

}

#' Check whether GUID/UUID is valid
#'
#' Returns an error message if GUID/UUID is invalid
#'
#' @param guid GUID value. Character.
#' @param has_hyphens Logical. Determines pattern used for evalauting whether GUID is of valid form.
#' @param fail_msg Message to display if GUID is not valid
#'
#' @return Error message
#'
#' @importFrom assertthat assert_that
#'
#' @noRd
check_guid <- function(
    guid,
    has_hyphens = FALSE,
    fail_msg
) {

    assertthat::assert_that(
        is_guid(guid = guid, has_hyphens = has_hyphens ),
        msg = fail_msg
    )

}

#' Checks whether is a valid user name
#' 
#' Returns a Boolean value
#' 
#' @param name Character. User name to validate.
#' 
#' @importFrom stringr str_detect
#' 
#' @noRd 
is_user_name <- function(name) {

    # must be between 3 and 15 characters long
    nchar(name) %in% c(3:15) &&
    # contains only letters, numbers, and underscore symbol
    stringr::str_detect(string = name, pattern = "[^A-Za-z0-9_]+", negate = TRUE)

} 

# url exists
# is_url: string is a valid URL?

# stop if minimum set of parameters not provided
# consider {attempt} for stop_if and other checkers. See more CRAN links [here](https://cran.r-project.org/web/packages/attempt/index.html) and example in utils in step 4 [here](https://colinfay.me/build-api-wrapper-package-r/)

#' Convert logical values to lower-case strings
#'
#' For API endpoints that expect 'true' and 'false' in lieu of \code{TRUE} and \code{FALSE}, respectively, convert R's logical values into the lower-case values expected by the API.
#'
#' @param value Logical: c(TRUE, FALSE)
#'
#' @return Character: c("true", "false", "")
#'
#' @noRd
logical_to_string <- function(
    value
) {

    ifelse(
        # check whether value is logical
        test = value %in% c(TRUE, FALSE), 
        # if so, convert to lower-case character
        yes = ifelse(
            test = value == TRUE, 
            yes = "true", 
            no = "false"), 
        # if not, convert to empty character
        no = ""
    
    )

}

#' Checks validity of workspace name
#' 
#' @param Character Name of the workspace
#' 
#' @return Logical. `TRUE` if a valid name; `FALSE` otherwise
#' 
#' @noRd
is_workspace_name <- function(x) {

    # compute number of characters total and number matched
    n_char <- nchar(x)
    n_matches <- stringr::str_count(string = x, pattern = "[[:lower:][:digit:]]")

    # number of characters == number of matched characters
    (n_char %in% c(1:12)) & (n_matches == n_char)

}

#' Checks validity of workspace display name
#' 
#' @param Character Display name of the workspace
#' 
#' @return Logical. `TRUE` if a valid name; `FALSE` otherwise
#' 
#' @noRd
is_workspace_display_name <- function(x) {
    n_char <- nchar(x) 
    n_char > 0 & n_char <= 300
}

#' Utility function to check workspace parameter
#' 
#' Checks (1) that the workspace has a valid form and (2) that is one that the user is authorized to access
#' 
#' @param verbose Logical. If `verbose == TRUE`, return logical outcome and print message. Otherwise, be silent.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace to check.
#' @param user Character. API user name
#' @param password Character. API password
#' 
#' @importFrom assertthat assert_that
#' @importFrom glue glue glue_collapse
#' 
#' @noRd 
check_workspace_param <- function(
    server = Sys.getenv("SUSO_SERVER"),
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # invalid name
    assertthat::assert_that(
        is_workspace_name(workspace),
        msg = "Invalid workspace name. Please check the input for the `workspace` parameter."
    )

    # workspace does not exist
    workspace_user <- suppressMessages(
        susoapi::get_user_details(
            user_id = user,
            server = server,
            workspace = workspace,
            user = user,
            password = password
        )
    )
    assertthat::assert_that(
        !is.null(workspace_user),
        msg = glue::glue(
            'User `{user}` does not have access to workspace `{workspace}`.'
        )
    )

    # workspaces <- susoapi::get_workspaces()$Name
    # assertthat::assert_that(
    #     workspace %in% workspaces,
    #     msg = glue::glue(
    #         "Workspace either does not exist or cannot be accessed by this user.", 
    #         "Please use one of the workspaces to which the user has access: {glue::glue_collapse(workspaces, sep = ', ', last = ', ')}",
    #         .sep = "\n"
    #     )

    # )

}

#' Compose URL of GraphQL API
#'
#' @param server Character. Full server web address
#' (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#'
#' @return Character. URL of GraphQL API for the server
#'
#' @importFrom httr modify_url
#'
#' @noRd
make_graph_ql_url <- function(
    server = Sys.getenv("SUSO_SERVER")
) {

    graphql_url <- httr::modify_url(
        url = server,
        path = "graphql"
    )

    return(graphql_url)

}

#' Send a GraphQL query
#'
#' @param graph_ql_url Character. URL of GraphQL API on the server.
#' @param user Character. API user name.
#' @param password Character. API user's password.
#' @param query Character. GraphQL query string
#'
#' @return httr request object
#'
#' @importFrom httr POST add_headers
#' @importFrom jsonlite base64_enc
#'
#' @noRd
perform_graph_ql_query <- function(
    graph_ql_url,
    user,
    password,
    query
) {

    request <- httr::POST(
        url = graph_ql_url,
        # - use `add_headers` so I can add a named list of headers
        # - encode user-password pair
        httr::add_headers(
            Authorization = paste0(
                "Basic ",
                jsonlite::base64_enc(input = paste0(user, ":", password))
            )
        ),
        # transmit GraphQL query as the body of the post
        # consider it a query
        body = list(query = query),
        encode = "json"
    )

}

#' Get the Survey Solutions version on the server
#'
#' @description
#' Extracts software version number from the `.version` sub-page
#'
#' @param server Character. Full server web address
#' (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#'
#' @return Character. Survey Solutions version
#'
#' @importFrom httr modify_url GET status_code content
#'
#' @export
get_server_version <- function(
    server = Sys.getenv("SUSO_SERVER")
) {

    # compose URL
    version_page <- httr::modify_url(
        url = server,
        path = ".version"
    )
    
    # make a response
    response <- httr::GET(url = version_page)

    # get status code
    status <- httr::status_code(response)

    if (status == 200) {

        # extract content
        version <- httr::content(x = response, as = "text", encoding = "UTF-8")

        # if a length 1 character vector, return version
        if (class(version) == "character" & length(version) == 1) {

            return(version)

        # otherwise, issue error
        } else {

            stop(
                paste0(
                    "Could not extract the Survey Solutions version number ",
                    "from the server.",
                    "Please post an issue : ",
                    "https://github.com/arthur-shaw/susoapi/issues"
                )
            )
           
        }

    } else {

        stop(
            paste0(
                "Survey Solutions version not found where expected. ",
                "Please post an issue : ",
                "https://github.com/arthur-shaw/susoapi/issues"
            )
        )

    }


}

