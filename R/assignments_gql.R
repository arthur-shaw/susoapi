#TODO: Fetch calendarevent details

#' Get the total count of assignments
#'
#' (Preliminary) GraphQL implementation of `assignments(...)` endpoint.
#'
#' @param supervisor_id Character. User ID (GUID) of supervisor.
#' @param archived Query archived or non-archived assignments
#' @param where 'where' filter condition for query
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user API user name
#' @param password API password
#'
#' @return Total count of assignments on the server that meet the user-specified search criteria.
#'
#' @import ghql
#' @importFrom jsonlite base64_enc fromJSON
#' @importFrom glue glue double_quote backtick

#' @noRd
get_assignments_count_gql <- function(
  server = Sys.getenv("SUSO_SERVER"),     # full server address
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),         # API user name
  password = Sys.getenv("SUSO_PASSWORD"),  # API password
  archived=FALSE,
  where=""
) {

  # compose the GraphQL request client
  assignments_request <- ghql::GraphqlClient$new(
    url = paste0(server, "/graphql"),
    headers = list(authorization = paste0(
      "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
    )
  )

  #Update archived
  archivedql <- ifelse(archived,"true","false")

  # compose the query for all assignments

  # use string interpolation to pipe double-quoted workspace name into query
  qry <- ghql::Query$new()
  qry$query("assignments",
            glue::glue("{
            assignments (
                workspace: <glue::double_quote(workspace)>
                take: 1
                skip: 0
                where:  {
                       <glue::as_glue(where)>
                      }
            ) {
                filteredCount
            }
        }", .open = "<", .close = ">")
  )


  # send request
  assignments_result <- assignments_request$exec(qry$queries$assignments)

  # convert JSON payload to data frame
  assignments <- jsonlite::fromJSON(assignments_result, flatten = TRUE)

  # extract total number of assignments
  assignments_count <- assignments$data$assignments$filteredCount

  assignments_info <- list(assignments = assignments, assignments_count = assignments_count)

  return(assignments_info)

}


#' Get one chunk of assignments
#'
#' @param take_n Numeric. Number of maps to take in one request.
#' @param skip_n Numeric. Number of maps to skip when paging through results.
#' @param where 'where' filter condition for query
#' @param nodes Character vector. Names of attributes to fetch for each map
#' @param archived Query archived or non-archived assignments
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose maps to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API or admin user name for user that access to the workspace.
#' @param password Character. API or admin password
#'
#' @return Data frame. Maps
#'
#' @import ghql
#' @importFrom jsonlite base64_enc fromJSON
#' @importFrom glue glue double_quote backtick
#' @importFrom dplyr `%>%` pull select rename_with starts_with left_join group_by summarize
#' @importFrom purrr map_if discard map_int
#' @importFrom rlang .data is_empty
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest pivot_wider
#'
#' @noRd
get_assignments_by_chunk_gql <- function(
  take_n = 100,
  skip_n = 0,
  where="",
  nodes = c(
    "archived",
    "createdAtUtc",
    "email",
    "id",
    "interviewsNeeded",
    "receivedByTabletAtUtc",
    "responsibleId",
    "webMode",
    "calendarEvent"
  ),
  archived=FALSE, # If archived should be captured as well
  server = Sys.getenv("SUSO_SERVER"),     # full server address
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),         # API user name
  password = Sys.getenv("SUSO_PASSWORD")  # API password
) {


  # compose the GraphQL request client
  assignments_request<- ghql::GraphqlClient$new(
    url = paste0(server, "/graphql"),
    headers = list(authorization = paste0(
      "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
    )
  )

  # determine whether requested calendarevent
  has_ce <- "calendarEvent" %in% nodes
  # expand users node if relevant
  if (has_ce) {
    nodes[which(nodes == "calendarEvent")] <-
      "calendarEvent {
            assignmentId
            comment
            creatorUserId
            interviewId
            interviewKey
            isCompleted
            publicKey
            startTimezone
            startUtc
            updateDateUtc
        }"
  }

  # expand users node if relevant
  # compose the query for all maps
  # use string interpolation to pipe double-quoted workspace name into query
  #Determine if archived or not and translate to graphql
  archivedql <- ifelse(archived,"true","false")

  qry <- ghql::Query$new()

  qry$query("assignments",
            stringr::str_squish((glue::glue("{
            assignments (
                workspace: <glue::double_quote(workspace)>
                take: <take_n>
                skip: <skip_n>
                where:  {
                       <glue::as_glue(where)>
                }

            ) {
                nodes {
                    <paste0(nodes, collapse = '\\n')>
                }
                filteredCount
            }
        }", .open = "<", .close = ">")))
  )



  # send request
  assignments_result <- assignments_request$exec(qry$queries$assignments)

  # convert JSON payload to data frame
  assignments <- jsonlite::fromJSON(assignments_result, flatten = TRUE)

  # extract number of maps returned in request
  assignment_count <- assignments$data$assignments$filteredCount

  if ("errors" %in% names(assignments)) {

    # extract and display error(s)
    errors <- dplyr::pull(assignments$errors) %>% paste0(collapse = "\n")
    stop(errors)

  } else if (assignment_count == 0) {

    message(glue::glue(
      "No assignments found in workspace {glue::backtick(workspace)}.",
      "If this result is surprising, check the input in the `workspace` parameter.",
      .sep = "\n"
    ))

  } else if (assignment_count > 0) {

    # extract map data payload
    assignments_df <- assignments$data$assignments$nodes %>%
      purrr::map_if(is.data.frame, list) %>%
      tibble::as_tibble()

    # if (has_ce) {
    #
    #   # extract assignments attributes from the payload
    #   id_cols <- names(assignments_df) %in% c("calendarEvent")
    #   assignments_attribs_df <- dplyr::select(assignments_df, -.data$calendarEvent)
    #
    #   # extract (nested) identifying data
    #   users_df <- assignments_df %>%
    #     dplyr::select(.data$fileName, .data$calendarEvent) %>%
    #     purrr::discard(rlang::is_empty) %>%
    #     purrr::map_if(is.data.frame, list) %>%
    #     tibble::as_tibble() %>%
    #     tidyr::unnest(.data$users) %>%
    #     #Reshape to match SuSo format
    #     dplyr::group_by(fileName) %>%
    #     dplyr::summarize(users = paste(userName, collapse = ','))
    #
    #   # combine map attributes and identifying data
    #   map_list_df <- assignments_attribs_df %>%
    #     dplyr::left_join(users_df, by = "fileName")
    #
    # } else if (has_ce == FALSE) {

    assignments_list_df <- assignments_df

    # }

    return(assignments_list_df)

  }

}


#' Get all assignments
#'
#' Get all assignments for query parameters. (Preliminary) GraphQL implementation of `assignments(...)` endpoint.
#'
#' @param qnr_id Questionnaire ID. GUID provided by the server.
#' @param qnr_version Questionnaire version. Version number provided by the server.
#' @param nodes Character vector. Names of attributes to fetch for each map
#' @param archived Query archived or non-archived assignments
#' @param chunk_size Numeric. Number of records to take in one request.
#' @param server Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose assignments to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user User name
#' @param password Password
#'
#' @return Data frame of all assignments that meet search criteria.
#'
#' @importFrom assertthat assert_that is.string is.flag
#' @importFrom purrr map_dfr map_chr
#'
#' @export
get_assignments_gql <- function(
  qnr_id="",
  qnr_version =NULL,
  nodes = c(
    "archived",
    "createdAtUtc",
    "email",
    "id",
    "interviewsNeeded",
    "receivedByTabletAtUtc",
    "responsibleId",
    "webMode",
    "calendarEvent"
  ),
  archived=FALSE,
  chunk_size = 100,
  server = Sys.getenv("SUSO_SERVER"),     # full server address
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),         # API user name
  password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

  # check inputs

  # workspace:
  # - invalid name
  # - workspace does not exist
  check_workspace_param(workspace = workspace)

  assertthat::assert_that(
    is_guid(qnr_id) ,
    msg = "Invalid `qnr_id`. The value must be  a valid GUID."
  )

  # nodes in known list
  nodes_allowed = c(
    "archived",
    "createdAtUtc",
    "email",
    "id",
    "interviewsNeeded",
    "receivedByTabletAtUtc",
    "responsibleId",
    "webMode",
    "calendarEvent"
  )

  assertthat::assert_that(
    all(nodes %in% nodes_allowed),
    msg = "Invalid node listed in `node`. See documentation for allowed nodes."
  )

  # nodes must contain `id`
  if (!"id" %in% nodes) {
    stop("The requested nodes must contain `id`.")
  }

  #Determine if archived or not and translate to graphql
  archivedql <- ifelse(archived,"true","false")

  #Make conditional 'where' filter since AssignmentsFilter 'QuestionnaireIdentiy-version:' doesn't allow null
  #So either just questionnaire identiy or also version
  #TODO: Beautify, to allow multiple 'where'
  where_filter <- paste0(c(glue::glue("archived: {eq: <glue::as_glue(archivedql)> }",
                                      " questionnaireId: { id: {eq: <glue::double_quote(qnr_id)>}", .open = "<", .close = ">"),

                           ifelse(
                             test=qnr_version!="",
                             yes=glue::glue("} version: {eq: <glue::as_glue(qnr_version)>}}",.open = "<", .close = ">"),
                             no="}")
  ) , collapse = '')


  # get total count of assignments
  assignments_info <- get_assignments_count_gql(
    where=where_filter,
    workspace = workspace,
    server = server,
    user = user,
    password = password,
    archived = archived
  )

  # case 1: handle "errors"
  # if request returns errors
  if ("errors" %in% names(assignments_info$assignments)) {

    # extract and display error(s)
    errors <- dplyr::pull(assignments_info$assignments$errors) %>% paste0(collapse = "\n")
    stop(errors)

    # if no assignments found
  } else if (assignments_info$assignments_count == 0) {

    message(glue::glue(
      "No assignments found in workspace {glue::backtick(workspace)}.",
      "If this result is surprising, check the input in the `workspace` parameter.",
      .sep = "\n"
    ))

    # case 2: handle successfull assignments request
  } else if (assignments_info$assignments_count > 0) {

    # page through assignments; compile data
    assignments <- purrr::map_dfr(
      .x = seq(from = 0, to =assignments_info$assignments_count , by = chunk_size), #
      .f = ~ get_assignments_by_chunk_gql(
        workspace = workspace,
        where=where_filter,
        take_n = chunk_size,
        skip_n = .x,
        nodes = nodes,
        archived = archived,
        server = server,
        user = user,
        password = password
      )
    )

    return(assignments)

  }

}


