
#' Get list of export jobs
#'
#' Get data frame of export jobs on the server. This includes jobs that have been started, cancelled, and completed. The data frame includes the job id as well as several attributes about the job.
#'
#' Wrapper for \code{GET /api/v2/export} endpoint
#'
#' @param export_type Character. Type of data to export. Values: \code{c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Binary")}
#' @param interview_status Character. Status of interviews to export. Values: \code{c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")}
#' @param qnr_id Character. Questionnaire ID. Format: \code{qnr_id$version}
#' @param export_status Character. Status of export process. Values: \code{c("Created", "Running", "Completed", "Fail", "Canceled")}
#' @param has_file Logical. Whether export file generated. 
#' @param limit Number of results to return. Values: \code{c(1:40)}
#' @param offset Offset in list of processes.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose export jobs to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Data frame of export jobs.
#'
#' @importFrom assertthat assert_that is.flag is.number
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @export
get_export_jobs <- function(
    export_type = "", # values: c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata")
    interview_status = "All", # values: c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")
    qnr_id = "", # format qnr_id$version
    export_status = "", # values: c("Created", "Running", "Completed", "Fail", "Canceled")
    has_file = NA, 
    limit = 40, # limit %in% c(1, 40)
    offset = 0, #
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # check inputs
    # workspace
    check_workspace_param(workspace = workspace)

    # export_type
    if (export_type != "") {
        assertthat::assert_that(
            export_type %in% c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata"),
            msg = "Invalid `export_type` value. Must be one of the following: c('Tabular', 'STATA', 'SPSS', 'Binary', 'DDI', 'Paradata')"
        )
    }

    # interview_status
    if (interview_status != "") {
        assertthat::assert_that(
            interview_status %in% c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters"),
            msg = "Invalid `interview_status` value. Must be one of the following: c('All', 'SupervisorAssigned', 'InterviewerAssigned', 'Completed', 'RejectedBySupervisor', 'ApprovedBySupervisor', 'RejectedByHeadquarters', 'ApprovedByHeadquarters')"
        )
    }

    # qnr_id
    if (qnr_id != "") {

        # split questionnaire ID into analyzable parts
        grepl(pattern = "\\$", x= qnr_id)
        qnr <- strsplit(qnr_id, split = "\\$")[[1]][1]
        id <- strsplit(qnr_id, split = "\\$")[[1]][2]

        # check that part before $ is a guid and that part after is a number
        assertthat::assert_that(
            is_guid(qnr) & assertthat::is.number(as.numeric(id)),
            msg = "`qnr_id` is not a valid questionnaire ID in the format `id$version`"
        )

    }

    # export_status
    if (export_status != "") {
        assertthat::assert_that(
            export_status %in% c("Created", "Running", "Completed", "Fail", "Canceled"),
            msg = "Invalid `export_status` value. Must be one of the following: c('Created', 'Running', 'Completed', 'Fail', 'Canceled')"
        )
    }

    # has_file
    if (!has_file %in% c(NA, TRUE, FALSE)) {
        assertthat::assert_that(
            assertthat::is.flag(has_file),
            msg = "`has_file` must be `TRUE` or `FALSE` or `NA`."
        )
    }

    # form base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v2/export")
    )

    # form the query parameters of the request
    query <- list(
        exportType = export_type,
        interviewStatus = interview_status,
        questionnaireIdentity = qnr_id,
        exportStatus = export_status,
        hasFile = logical_to_string(has_file),
        limit = limit,
        offset = offset
    )
    query <- query[query != ""]

    # compose the full URL: base + query parameters
    url <- httr::modify_url(url = base_url, query = query)

    # get assignments from the server
    response <- httr::GET(
        url = url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # return list of jobs, converting empty list into empty df
    export_list <- jsonlite::fromJSON(content(response, as = "text"))
    if (length(export_list) == 0) {
        message("No export processes found matching the query paramters. Empty data frame returned")
        export_list <- data.frame(
            JobId = NA_real_,
            ExportStatus = NA_character_,
            StartDate = NA_character_,
            CompleteDate = NA_character_,
            Progress = NA_real_,
            ETA = NA_real_,
            Error = NA_character_,
            Links = NA_character_,
            HasExportFile = NA,
            ExportType = NA_character_,
            QuestionnaireId = NA_character_,
            InterviewStatus = NA_character_,
            From = NA_character_,
            To = NA_character_,
            AccessToken = NA_character_,
            RefreshToken = NA_character_,
            StorageType = NA_character_,
            TranslationId = NA_character_,
            IncludeMeta = NA,
            stringsAsFactors = FALSE
        )
    }

    return(export_list)

}

#' Start export file creation
#'
#' Has server-side side-effect of starting an export job for the target questionnaire. The function parameters provide a way to describe which interviews should be exported and for which questionnaire.
#'
#' Wrapper for \code{POST /api/v2/export} endpoint
#'
#' @param qnr_id Character. Questionnaire ID. Format: \code{qnr_id$version}
#' @param export_type Character. Type of data to export. Values: \code{c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata")}
#' @param interview_status Character. Status of interviews to export. Values: \code{c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")}
#' @param from Character. Start date for interviews to export. date-time, UTC
#' @param to Character. End date for interviews to export. date-time, UTC
#' @param access_token Access token to external storage. Relevant only if external cloud storage is destination for the export file.
#' @param refresh_token Refresh token to external storage.Relevant only if external cloud storage is destination for the export file.
#' @param storage_type Character. External storage type, if relevant. Values: \code{c("Dropbox", "OneDrive", "GoogleDrive")}
#' @param translation_id Character. Translation ID for variable and value labels to include in export files.
#' @param include_meta Logical. If `TRUE`, include questionnaire metadata in export file.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose export jobs to start. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return Job ID of export process started.
#'
#' @import httr
#' @importFrom jsonlite fromJSON
#'
#' @export
start_export <- function(
    qnr_id,
    export_type,
    interview_status = "All",
    from = "",
    to = "",
    access_token = "",
    refresh_token = "",
    storage_type = "",
    translation_id = "",
    include_meta = TRUE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # check inputs

    # workspace
    check_workspace_param(workspace = workspace)

    # export_type
    assertthat::assert_that(
        export_type %in% c("Tabular", "STATA", "SPSS", "Binary", "DDI", "Paradata"),
        msg = "Invalid `export_type` value. Must be one of the following: c('Tabular', 'STATA', 'SPSS', 'Binary', 'DDI', 'Paradata')"
    )

    # qnr_id

    # split questionnaire ID into analyzable parts
    grepl(pattern = "\\$", x= qnr_id)
    qnr <- strsplit(qnr_id, split = "\\$")[[1]][1]
    id <- strsplit(qnr_id, split = "\\$")[[1]][2]

    # check that part before $ is a guid and that part after is a number
    assertthat::assert_that(
        is_guid(qnr) & assertthat::is.number(as.numeric(id)),
        msg = "`qnr_id` is not a valid questionnaire ID in the format `id$version`"
    )

    # interview_status
    assertthat::assert_that(
        interview_status %in% c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters"),
        msg = "Invalid `interview_status` value. Must be one of the following: c('All', 'SupervisorAssigned', 'InterviewerAssigned', 'Completed', 'RejectedBySupervisor', 'ApprovedBySupervisor', 'RejectedByHeadquarters', 'ApprovedByHeadquarters')"
    )

    # from
    if (from != "") {
        assertthat::assert_that(
            !is.na(suppressWarnings(lubridate::ymd(from))),
            msg = "Invalid date provided in `from`. Make sure that the date is valid and follows YYYY-MM-DD format."
        )
    }

    # to
    if (from != "") {
        assertthat::assert_that(
            !is.na(suppressWarnings(lubridate::ymd(to))),
            msg = "Invalid date provided in `to`. Make sure that the date is valid and follows YYYY-MM-DD format."
        )
    }

    # storage_type
    if (storage_type != "") {
        assertthat::assert_that(
            storage_type %in% c("Dropbox", "OneDrive", "GoogleDrive"),
            msg = "Invalid `storage_type` value. Must be one of the following: c('Dropbox', 'OneDrive', 'GoogleDrive')"
        )
    }

    # translation_id
    if (translation_id != "") {
        check_guid(
            guid = translation_id,
            fail_msg = "`translation_id` is not a valid GUID. Please check.")
    }

    # include_meta
    assertthat::assert_that(
        assertthat::is.flag(include_meta),
        msg = "Invalid `include_meta` value. Must be: c(TRUE, FALSE)."
    )

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v2/export")
    )

    # form the body for the export request, excluding empty elements of list
    body <- list(
        ExportType = export_type, # Tabular, STATA, SPSS, Binary, DDI, Paradata
        QuestionnaireId = qnr_id,	# Questionnaire id in format [QuestionnaireGuid$Version]
        InterviewStatus = interview_status,	# All, SupervisorAssigned, InterviewerAssigned, Completed, RejectedBySupervisor, ApprovedBySupervisor, RejectedByHeadquarters, ApprovedByHeadquarters
        From = from,	# date-time, UTC
        To = to,	# date-time, UTC
        AccessToken = access_token,	# Access token to external storage
        RefreshToken = refresh_token, # Refresh token to external storage
        StorageType	= storage_type, # Dropbox, OneDrive, GoogleDrive
        TranslationId = translation_id, # Translation Id of the questionnaire
        IncludeMeta = logical_to_string(include_meta)
    )
    body <- body[body != ""]

    # post request
    response <- httr::POST(
        url = base_url,
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # return job ID of started export process
    status <- httr::status_code(response)
    # export started
    if (status == 201) {
        job_id <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)$JobId
        message(paste0("Export job successfully started with job ID ", job_id))
    # questionnaire ID malformed
    } else if (status == 400) {
        job_id <- NA_integer_
        warning(
            paste0(
                "Export job not started.\n",
                "Questionnaire ID is malformed.\n",
                "ID provided in qnr_id: ", qnr_id)
            )
    } else if (status == 404) {
        job_id <- NA_integer_
        warning(
            paste0(
                "Export job not started.\n",
                "Questionnaire with ID ", qnr_id, " not found.")
            )
    } else if (!status %in% c(201, 400, 404)) {
        job_id <- NA_integer_
        warning(
            paste0(
                "Export job not started.\n",
                "Unknown error arose. HTTP code: ", status, ".")
            )
    }

    return(job_id)

}

#' Get detailed information about export process
#'
#' Get data frame of export job details for a single job. This provides a nice mechanism to inquire about the progress of recently started export jobs and see if they are complete.
#'
#' Wrapper for \code{GET /api/v2/export/{id}} endpoint
#'
#' @param job_id Numeric. Export job ID
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose export job details to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return If job found, data frame of export job details. Otherwise, message about why the job was not found.
#'
#' @export
#'
#' @importFrom assertthat assert_that is.number
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom purrr list_modify modify_if flatten_df
get_export_job_details <- function(
    job_id,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # check inputs
    # job_id
    assertthat::assert_that(
        assertthat::is.number(job_id),
        msg = "Invalid `job_id` value provided."
    )

    # workspace
    check_workspace_param(workspace = workspace)

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v2/export/", job_id)
    )

    # send and get response
    response <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # return details
    status <- httr::status_code(response)

    if (status == 200) {

        # extract from response
        export_details <- jsonlite::fromJSON(content(response, as = "text"), flatten = TRUE)

        # flatten extracted list to data frame
        cancel_link <- export_details[["Links"]]$Cancel # extract cancel link from list
        download_link <- export_details[["Links"]]$Download # extract download link from list
        export_details[["Links"]] <- NULL # remove Links list from list
        export_details <- purrr::list_modify( # add cancel and download link entries to list
            .x = export_details,
            CancelLink = ifelse(is.null(cancel_link), NA_character_, cancel_link), # replace null with NA
            DownloadLink = ifelse(is.null(download_link), NA_character_, download_link) # replace null with NA
        )
        export_details <- purrr::modify_if(export_details, .p = is.null, .f = ~ NA) # replace NULL with NA
        export_details <- purrr::flatten_df(export_details)

        # return details as df
        return(export_details)

    } else if (status == 404) {

        message(paste0(
            "Details could not be found for export job ", job_id, ".\n",
            "Export job could not be found. HTTP code 404."))

    } else if (!status %in% c(200, 400)) {

        message(paste0(
            "Details could not be found for export job ", job_id, ".\n",
            "Unknown issue arose. HTTP code ", status, "."))

    }

}

#' Cancel export process
#'
#' Has server-side side-effect of cancelling an active export process. A message will inform about the outcome. If `verbose = TRUE`, the function also returns a logical value: `TRUE` if cancellation successful; `FALSE` otherwise.
#'
#' Wrapper for \code{DELETE /api/v2/export/{id}} endpoint
#'
#' @param job_id Numeric. Export job ID
#' @param verbose Logical. If `verbose = TRUE`, return `TRUE` if cancellation successful, `FALSE` otherwise
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose export jobs to cancel. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return If `verbose = FALSE`, no return value. If `verbose = TRUE`, return logical outcome.
#' @export
#'
#' @import httr
cancel_export <- function(
    job_id,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # check inputs

    # job_id
    assertthat::assert_that(
        assertthat::is.number(job_id),
        msg = "Invalid `job_id` value provided."
    )

    # verbose
    assertthat::assert_that(
        verbose %in% c(TRUE, FALSE),
        msg = "Invalid `verbose` value provided. Must be `TRUE` or `FALSE`."
    )

    # workspace
    check_workspace_param(workspace = workspace)

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v2/export/", job_id)
    )

    # send request
    response <- httr::DELETE(
        url = base_url,
        httr::authenticate(user = user, password = password),
		httr::accept_json(),
		httr::content_type_json()
    )

    # return result of operation
    status <- httr::status_code(response)

    if (status == 200) {

        result <- TRUE
        message(paste0("Export job ", job_id, " successfully cancelled"))

    } else if (status == 404) {

        result <- FALSE
        message(paste0(
            "Details could not be found for export job ", job_id, ".\n",
            "Export job could not be found. HTTP code 404."))

    } else if (!status %in% c(200, 400)) {

        result <- FALSE
        message(paste0(
            "Details could not be found for export job ", job_id, ".\n",
            "Unknown issue arose. HTTP code ", status, "."))

    }

    if (verbose == TRUE) {
        return(result)
    }

}


#' Downloads export file.
#'
#' If the export file is ready, this function downloads it to the target directory specified in `path`. If the file is not ready, print a message describing the situation.
#'
#' Wrapper for \code{GET /api/v2/export/{id}/file} endpoint
#'
#' @param job_id Numeric. Export job ID
#' @param path Character. File path where export file should be downloaded
#' @param verbose Logical. If `verbose = TRUE`, return a logical value about the outcome. If `verbose = FALSE`, the default, simply return a message describing the outcome.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose export files to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API user name
#' @param password Character. API password
#'
#' @return Logical. \code{TRUE} if file successfully downloaded; \code{FALSE} otherwise.
#'
#' @export
#'
#' @importFrom assertthat assert_that is.number
#' @import httr
#' @importFrom stringr str_match
#' @importFrom fs path
get_export_file <- function(
    job_id,
    path,
    verbose = FALSE,
    server = Sys.getenv("SUSO_SERVER"),     # full server address
    workspace = Sys.getenv("SUSO_WORKSPACE"),
    user = Sys.getenv("SUSO_USER"),         # API user name
    password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

    # check inputs
    # job_id
    assertthat::assert_that(
        assertthat::is.number(job_id),
        msg = "Invalid `job_id` value provided."
    )

    # path
    assertthat::assert_that(
        dir.exists(path), # assertthat::is.dir does not seem to work; using base r replacement
        msg = "Download path specified in `path` is not a valid directory."
    )

    # verbose
    assertthat::assert_that(
        verbose %in% c(TRUE, FALSE),
        msg = "Invalid `verbose` value provided. Must be `TRUE` or `FALSE`."
    )

    # workspace
    check_workspace_param(workspace = workspace)

    # form the base URL
    base_url <- httr::modify_url(
        url = server, 
        path = paste0(workspace, "/api/v2/export/", job_id, "/file")
    )

    # request redirect link
    response_redir <- httr::GET(
        url = base_url,
        httr::authenticate(user = user, password = password),
  		httr::accept_json(),
  		httr::content_type_json(),
        httr::config(               # use curl options to:
            followlocation = 0L     # NOT follow redirects
        )
    )

    # extract file name from response header
    file_name <- stringr::str_match(httr::headers(response_redir)$`content-disposition`, "(?<=filename=).+(?=;)")

    status <- httr::status_code(response_redir)

    # if file available, get it
    if (status == 200) {

        response <- httr::GET(
            url = base_url,
            httr::authenticate(user = user, password = password),
            httr::accept_json(),
            httr::content_type_json(),
            httr::write_disk(fs::path(path, file_name), overwrite = TRUE)
        )

        # extract file name from response header
        # file_name <- stringr::str_match(httr::headers(response)$`content-disposition`, "(?<=filename=).+(?=;)")

        # rename downloaded file to name from the response header
        # file.rename(from = fs::path(path, "temp.zip"), to = fs::path(path, file_name))

        # emit message about outcome, path, and file name
        message(paste0(
            "File successfully downloaded to:\n",
            fs::path(path, file_name)
        ))
        result <- TRUE

    # if file at redirect link, follow link
    } else if (status == 302) {

        # extract redirect link from first request
        redirect_url <- response_redir$headers$location

        # follow redirect url and download file
        response <- httr::GET(
            url = redirect_url,
            httr::write_disk(fs::path(path, "temp.zip"), overwrite = TRUE)
        )

        # extract file name from response header
        file_name <- stringr::str_extract(
            string = headers(response)$`content-disposition`, # header component with file name
            pattern = '(?<=\\").+(?=\\")') # name between quotes

        # rename downloaded file to name from the response header
        file.rename(from = fs::path(path, "temp.zip"), to = fs::path(path, file_name))

        # emit message about outcome, path, and file name
        message(paste0(
            "File successfully downloaded to:\n",
            fs::path(path, file_name)
        ))

        result <- TRUE

    # file not generated yet
    } else if (status == 400) {

        message(paste0("Export file not yet generated for export job ", job_id))
        result <- FALSE

    # export process not found
    } else if (status == 404) {

        message(paste0("Export job ", job_id, " could not be found. \nHTTP code: 404."))
        result <- FALSE

    # other result
    } else if (!status %in% c(200, 302, 400, 404)) {

        message(paste0("Unexpected result for job ", job_id, ". HTTP code: ", status, "."))
        result <- FALSE

    }

    if (verbose == TRUE) {
        return(result)
    }

    # NOTE: not sure why unrestricted_auth does not seem to work
    # response <- httr::GET(
    #     url = base_url,
    #     authenticate(user = user, password = password),
  	# 	accept_json(),
  	# 	content_type_json(),
    #       write_disk(paste0(path, "temp.zip"), overwrite = TRUE),
    #       config(                     # use curl options to:
    #           followlocation = 1L,        # follow redirects
    #           unrestricted_auth = 0L      # but not pass auth to redirects
    #       )
    #   )

}

