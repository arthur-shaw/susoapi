#' Set server credentials
#'
#' Stores server access credentials for later use. For the current session, credentials are accessible in environment variables. For future sessions, credentials are stored in the .Renviron and loaded upon startup. While functions in this package automatically load the credential set in this way, the credentials can also be accessed via the `Sys.getenv` function and using the credential name SUSO_SERVER (server address), SUSO_USER (API user name), SUSO_PASSWORD (API user password)
#'
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param user API user name
#' @param password API password
#'
#' @return Environment variables `SUSO_SERVER`, `SUSO_USER`, `SUSO_PASSWORD`
#' 
#' @export
set_credentials <- function(
    server,
    user,
    password
) {

    # check for .Renviron file
    home_path <- Sys.getenv("HOME")
    renv_path <- file.path(home_path, ".Renviron")

    # if file doesn't exist, create it
    if (!file.exists(renv_path)) {

        file.create(renv_path)
        message(paste0(
            "No .Renviron file found. File created at following address: "),
            renv_path
        )

    }

    # if file does exist, remove old SuSo entries, if they exist
    else if (file.exists(renv_path)) {

        old_env_vars <- readLines(con = renv_path) # ingest .Renviron file
        suso_env_entries <- grep(pattern = "SUSO_", x = old_env_vars)
        new_env_vars <- ifelse(
            test = length(suso_env_entries) > 0,
            yes = old_env_vars[-grep(pattern = "SUSO_", x = old_env_vars)],
            no = old_env_vars
        )
        writeLines(text = new_env_vars, con = renv_path)

    }

    # write credentials to .Renviron
    # construct entries
    server_entry <- paste0("SUSO_SERVER = '", server, "'")
    user_entry <- paste0("SUSO_USER = '", user, "'")
    password_entry <- paste0("SUSO_PASSWORD = '", password, "'")
    # append entries to .Renviron
    write(server_entry, file = renv_path, append = TRUE, sep = "\n")
    write(user_entry, file = renv_path, append = TRUE, sep = "\n")
    write(password_entry, file = renv_path, append = TRUE, sep = "\n")

    # write credentials to environment vars too, for use in same session as authorization set
    Sys.setenv(
        SUSO_SERVER = server,
        SUSO_USER = user,
        SUSO_PASSWORD = password
    )

    # inform the user that credentials have been set and what other actions, optionally, to take
    message(paste0(
        "Credentials successfully set.\n",
        "They are available for immediate use by `susoapi` functions, or other packages that use `susoapi`.\n",
        "They are also available for future use, upon startup in any new R session.\n",
        "Use `show_credentials` to see them.\n",
        "Use `check_credentials` to check them for the server.\n",
        "Use `set_credentials` to change them."
    ))

}

#' Show saved server credentials
#'
#' Shows server credentials saved in environment variables and .Renviron.
#' 
#' @return Side-effect of message in console
#' @export
#'
#' @examples
show_credentials <- function() {
    message(
        paste(
        "Currently saved credentials are:",
        paste0("- server: ", Sys.getenv("SUSO_SERVER")),
        paste0("- user: ", Sys.getenv("SUSO_USER")),
        paste0("- password: ", Sys.getenv("SUSO_PASSWORD")),
        sep = "\n") 
    )
}

#' Check that server credentials are valid
#'
#' Shows server credentials saved in environment variables and .Renviron.
#' @param workspace Character. Name of the workspace for which credentials will be checked.
#' @param verbose Logical. If `TRUE`, function returns logical regarding validity of credentials. If `FALSE`, function simply prints information about validity to the console.
#' 
#' @return If `verbose = FALSE` (default), side-effect of message printed to the console. If `verbose = TRUE`, logical: `TRUE` if credentials valid; `FALSE` otherwise.
#' 
#' @import httr
#' @importFrom glue glue_collapse backtick glue
#' 
#' @export
check_credentials <- function(
    workspace = "primary",
    verbose = FALSE
) {

    # extract server, user, and password environment variables
    server      <- Sys.getenv("SUSO_SERVER")
    user        <- Sys.getenv("SUSO_USER")
    password    <- Sys.getenv("SUSO_PASSWORD")

    # check whether credentials are completely non-missing
    if (server == "" | user == "" | password == "") {
        
        message(paste0(
            "Credentials are missing, either entirely or partially.",
            "\nUse `set_credentials` to specify complete and correct credentials."))
        
        credentials_valid <- FALSE

    # check whether credentials are valid
    # by making getting the workspaces for which the user is authorized
    } else {

        workspaces_df <- tryCatch(
            error = function(cnd) {
                data.frame(Name = NA_character_)
            },
            susoapi::get_workspaces(server = server, user = user, password = password)
        )

        workspaces_names <- workspaces_df$Name

        credentials_valid <- (workspace %in% workspaces_names)

        if (credentials_valid == TRUE) {
            workspaces_list <- glue::glue_collapse(glue::backtick(workspaces_names), sep = ", ", last = "and ")
            message(glue::glue("Credentials valid for workspace(s) {workspaces_list}."))
        } else {
            message(glue::glue(
                "Credentials invalid for workspace {glue::backtick(workspace)}.",
                "Here are some steps to troubleshoot.", 
                "First, `show_credentials()` to view the credentials.", 
                "If they are incorrect, use use `set_credentials()` to correct them.",
                "Next, check which workspace(s) the user can access.",
                "To do so, use `get_workspaces()`.",
                .sep = "\n"
            ))
                
        }

    }

    if (verbose == TRUE) {
        return(credentials_valid)
    }

}
