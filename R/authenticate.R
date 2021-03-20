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

    # check that credentials work


    message("SuSo server access credentials have been saved \nFor immediate use, they are available as environment variables. \nFor future use, they are stored in .Renviron. \nFrom there, they will be loaded into environment variables upon startup.")

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
#'
#' @param verbose Logical. If `TRUE`, function returns logical regarding validity of credentials. If `FALSE`, function simply prints information about validity to the console.
#' 
#' @return If `verbose = FALSE` (default), side-effect of message printed to the console. If `verbose = TRUE`, logical: `TRUE` if credentials valid; `FALSE` otherwise.
#' 
#' @import httr
#' 
#' @export
check_credentials <- function(
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
    # by making the smallest possible query: returning 1 questionnaire from the server
    } else {

        # questionnaire list API path
        path <- "/api/v1/questionnaires"

        # query for 1 questionnaire with no offset
        query <- list(
            limit = 1, 
            offset = 1
        )

        # construct url
        url <- httr::modify_url(
                url = server, 
                path = path, 
                query = query)

        test_credentials <- httr::GET(
            url = url,
            authenticate(user = user, password = password),
            accept_json(),
            content_type_json()        
        )

        credentials_valid <- httr::status_code(test_credentials) == 200

        if (credentials_valid == TRUE) {
            message("Credentials valid.")
        } else {
            message(paste0(
                "Credentials invalid.", 
                "\nFirst, `show_credentials` to see and check input credentials.", 
                "\nThen, use `set_credentials` to provide correct credentials."
            ))
                
        }

    }

    if (verbose == TRUE) {
        return(credentials_valid)
    }

}
