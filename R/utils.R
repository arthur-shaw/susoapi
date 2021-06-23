
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
#' @param workspace Character. Name of the workspace to check.
#' 
#' @importFrom assertthat assert_that
#' @importFrom glue glue glue_collapse
#' 
#' @noRd 
check_workspace_param <- function(workspace) {

    # invalid name
    assertthat::assert_that(
        is_workspace_name(workspace),
        msg = "Invalid workspace name. Please check the input for the `workspace` parameter."
    )

    # workspace does not exist
    workspaces <- susoapi::get_workspaces()$Name
    assertthat::assert_that(
        workspace %in% workspaces,
        msg = glue::glue(
            "Workspace either does not exist or cannot be accessed by this user.", 
            "Please use one of the workspaces to which the user has access: {glue::glue_collapse(workspaces, sep = ', ', last = ', ')}",
            .sep = "\n"
        )

    )

}
