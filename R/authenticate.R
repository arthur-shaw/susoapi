#' Set server credentials
#'
#' Stores server access credentials for later use. For the current session, credentials are accessible in environment variables. For future sessions, credentials are stored in the .Renviron and loaded upon startup. While functions in this package automatically load the credential set in this way, the credentials can also be accessed via the `Sys.getenv` function and using the credential name SUSO_SERVER (server address), SUSO_USER (API user name), SUSO_PASSWORD (API user password)
#'
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Workspace name. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return Environment variables `SUSO_SERVER`, `SUSO_WORKSPACE`, `SUSO_USER`, `SUSO_PASSWORD`
#' 
#' @export
set_credentials <- function(
    server,
    workspace,
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

    # if file does exist, remove old SuSo entries, if they exist
    } else if (file.exists(renv_path)) {

        # ingest .Renviron file
        old_env_vars <- readLines(con = renv_path)
        # identify lines that are old {susoapi} entries, if any
        suso_env_entries <- grep(pattern = "SUSO_", x = old_env_vars)
        # return REnviron entries without old {susoapi} entries
        # case 1: if there are entries, remove them
        if (length(suso_env_entries) > 0) {
            new_env_vars <-  old_env_vars[-suso_env_entries]
        # case 2: if there are no entries, keep all entries
        } else {
            new_env_vars <- old_env_vars
        }
        writeLines(text = new_env_vars, con = renv_path, sep = "\n")

    }

    # write credentials to .Renviron
    # construct entries
    server_entry <- paste0("SUSO_SERVER = '", server, "'")
    workspace_entry <- paste0("SUSO_WORKSPACE = '", workspace, "'")
    user_entry <- paste0("SUSO_USER = '", user, "'")
    password_entry <- paste0("SUSO_PASSWORD = '", password, "'")
    # append entries to .Renviron
    write(server_entry, file = renv_path, append = TRUE, sep = "\n")
    write(workspace_entry, file = renv_path, append = TRUE, sep = "\n")
    write(user_entry, file = renv_path, append = TRUE, sep = "\n")
    write(password_entry, file = renv_path, append = TRUE, sep = "\n")

    # write credentials to environment vars too, for use in same session as authorization set
    Sys.setenv(
        SUSO_SERVER = server,
        SUSO_USER = user,
        SUSO_PASSWORD = password,
        SUSO_WORKSPACE = workspace
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
show_credentials <- function() {
    message(
        paste(
        "Currently saved credentials are:",
        paste0("- server: ", Sys.getenv("SUSO_SERVER")),
        paste0("- workspace: ", Sys.getenv("SUSO_WORKSPACE")),
        paste0("- user: ", Sys.getenv("SUSO_USER")),
        paste0("- password: ", Sys.getenv("SUSO_PASSWORD")),
        sep = "\n") 
    )
}

#' Check that server credentials are valid
#'
#' Shows server credentials saved in environment variables and .Renviron.
#' 
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose users to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#' @param verbose Logical. If `TRUE`, function returns logical regarding validity of credentials. If `FALSE`, function simply prints information about validity to the console.
#' 
#' @return If `verbose = FALSE` (default), side-effect of message printed to the console. If `verbose = TRUE`, logical: `TRUE` if credentials valid; `FALSE` otherwise.
#' 
#' @import httr
#' @importFrom glue glue_collapse backtick glue
#' 
#' @export
check_credentials <- function(
    server = Sys.getenv("SUSO_SERVER"),
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),
    password = Sys.getenv("SUSO_PASSWORD"),
    verbose = FALSE
) {

    # check whether credentials are completely non-missing
    if (server == "" | workspace == "" | user == "" | password == "") {
        
        message(paste0(
            "Credentials are missing, either entirely or partially.",
            "\nUse `set_credentials` to specify complete and correct credentials."))
        
        credentials_valid <- FALSE

    # check whether credentials are valid
    # by making getting the workspaces for which the user is authorized
    } else {

        credentials_valid <- tryCatch(
            error = function(cnd) {
                FALSE
            },
                is.data.frame(
                    suppressMessages(
                        susoapi::get_user_details(
                            user_id = user,
                            server = server,
                            workspace = workspace,
                            user = user,
                            password = password
                        )
                    )
                )
        )

        if (credentials_valid == TRUE) {
            message(glue::glue("Credentials valid for workspace `{workspace}`."))
        } else {
            message(glue::glue(
                "Credentials invalid for workspace {glue::backtick(workspace)}.",
                "Here are some steps to troubleshoot.", 
                "First, `show_credentials()` to view the credentials.", 
                "If they are incorrect, use use `set_credentials()` to correct them.",
                "Next, as server admin, check which workspace(s) the user can access.",
                "If the target workspace cannot be accessed by the user, add the user.",
                .sep = "\n"
            ))
                
        }

    }

    if (verbose == TRUE) {
        return(credentials_valid)
    }

}
