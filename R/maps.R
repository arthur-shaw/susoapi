#' Get total count of maps uploaded in workspace
#'
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose maps to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API or admin user name for user that access to the workspace.
#' @param password Character. API or admin password
#'
#' @return List consisting of two element: maps request information and map count
#'
#' @import ghql
#' @importFrom jsonlite base64_enc fromJSON
#' @importFrom glue glue double_quote
#'
#' @noRd


get_maps_count <- function(
  server = Sys.getenv("SUSO_SERVER"),     # full server address
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),         # API user name
  password = Sys.getenv("SUSO_PASSWORD")  # API password
) {

  # compose the GraphQL request client
  maps_request <- ghql::GraphqlClient$new(
    url = paste0(server, "/graphql"),
    headers = list(authorization = paste0(
      "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
    )
  )

  # compose the query for all maps
  # use string interpolation to pipe double-quoted workspace name into query
  qry <- ghql::Query$new()
  qry$query("maps",
            glue::glue("{
            maps (
                workspace: <glue::double_quote(workspace)>
                take: 1
                skip: 0
            ) {
                filteredCount
            }
        }", .open = "<", .close = ">")
  )

  # send request
  maps_result <- maps_request$exec(qry$queries$maps)

  # convert JSON payload to data frame
  maps <- jsonlite::fromJSON(maps_result, flatten = TRUE)

  # extract total number of maps
  maps_count <- maps$data$maps$filteredCount

  maps_info <- list(maps = maps, maps_count = maps_count)

  return(maps_info)

}


#' Get chunk of maps uploaded in workspace returned from the server
#'
#' @param take_n Numeric. Number of maps to take in one request.
#' @param skip_n Numeric. Number of maps to skip when paging through results.
#' @param nodes Character vector. Names of attributes to fetch for each map
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


get_maps_by_chunk <- function(
  take_n = 100,
  skip_n = 0,
  nodes = c(
    "fileName",
    "size",
    "users",
    "importDateUtc",
    "xMaxVal",
    "yMaxVal",
    "xMinVal",
    "yMinVal",
    "wkid",
    "maxScale",
    "minScale"
  ),
  server = Sys.getenv("SUSO_SERVER"),     # full server address
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),         # API user name
  password = Sys.getenv("SUSO_PASSWORD")  # API password
) {


  # determine whether requested identifying data
  has_users <- "users" %in% nodes

  # compose the GraphQL request client
  maps_request<- ghql::GraphqlClient$new(
    url = paste0(server, "/graphql"),
    headers = list(authorization = paste0(
      "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password)))
    )
  )

  # expand users node if relevant
  if (has_users) {
    nodes[which(nodes == "users")] <-
      "users {
            userName
        }"
  }

  # expand users node if relevant
  # compose the query for all maps
  # use string interpolation to pipe double-quoted workspace name into query
  qry <- ghql::Query$new()
  qry$query("maps",
            stringr::str_squish((glue::glue("{
            maps (
                workspace: <glue::double_quote(workspace)>
                take: <take_n>
                skip: <skip_n>
            ) {
                nodes {
                    <paste0(nodes, collapse = '\\n')>
                }
                filteredCount
            }
        }", .open = "<", .close = ">")))
  )

  # send request
  maps_result <- maps_request$exec(qry$queries$maps)

  # convert JSON payload to data frame
  maps <- jsonlite::fromJSON(maps_result, flatten = TRUE)

  # extract number of maps returned in request
  maps_count <- maps$data$maps$filteredCount

  if ("errors" %in% names(maps)) {

    # extract and display error(s)
    errors <- dplyr::pull(maps$errors) %>% paste0(collapse = "\n")
    stop(errors)

  } else if (maps_count == 0) {

    message(glue::glue(
      "No maps found in workspace {glue::backtick(workspace)}.",
      "If this result is surprising, check the input in the `workspace` parameter.",
      .sep = "\n"
    ))

  } else if (maps_count > 0) {

    # extract map data payload
    maps_df <- maps$data$maps$nodes %>%
      purrr::map_if(is.data.frame, list) %>%
      tibble::as_tibble()

    if (has_users) {

      # extract maps attributes from the payload
      id_cols <- names(maps_df) %in% c("users")
      maps_attribs_df <- dplyr::select(maps_df, -.data$users)

      # extract (nested) identifying data
      users_df <- maps_df %>%
        dplyr::select(.data$fileName, .data$users) %>%
        purrr::discard(rlang::is_empty) %>%
        purrr::map_if(is.data.frame, list) %>%
        tibble::as_tibble() %>%
        tidyr::unnest(.data$users) %>%
        #Reshape to match SuSo format
        dplyr::group_by(fileName) %>%
        dplyr::summarize(users = paste(userName, collapse = ','))

      # combine map attributes and identifying data
      map_list_df <- maps_attribs_df %>%
        dplyr::left_join(users_df, by = "fileName")

    } else if (has_users == FALSE) {

      map_list_df <- maps_df

    }

    return(map_list_df)

  }

}

#' Get maps
#'
#' Get list of maps uploaded to workspace and their attributes
#'
#' GraphQL implementation of `maps(...)` endpoint.
#'
#' @param nodes Character vector. Names of attributes to fetch for each map
#' @param chunk_size Numeric. Number of records to take in one request.
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace whose maps to get. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API or admin user name for user that access to the workspace.
#' @param password Character. API or admin password
#'
#' @return Data frame of maps and their (user-specified) attributes.
#'
#' @importFrom assertthat assert_that
#' @importFrom purrr map_dfr
#'
#' @export


get_maps <- function(
  nodes = c(
    "fileName",
    "size",
    "users",
    "importDateUtc",
    "xMaxVal",
    "yMaxVal",
    "xMinVal",
    "yMinVal",
    "wkid",
    "maxScale",
    "minScale"
  ),
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

  # nodes in known list
  nodes_allowed = c(
    "fileName",
    "size",
    "users",
    "importDateUtc",
    "xMaxVal",
    "yMaxVal",
    "xMinVal",
    "yMinVal",
    "wkid",
    "maxScale",
    "minScale"
  )

  assertthat::assert_that(
    all(nodes %in% nodes_allowed),
    msg = "Invalid node listed in `node`. See documentation for allowed nodes."
  )

  # nodes must contain `fileName`
  if (!"fileName" %in% nodes) {
    stop("The requested nodes must contain `fileName`.")
  }

  # get total count of maps
  maps_info <- get_maps_count(
    workspace = workspace,
    server = server,
    user = user,
    password = password
  )

  # case 1: handle "errors"
  # if request returns errors
  if ("errors" %in% names(maps_info$maps)) {

    # extract and display error(s)
    errors <- dplyr::pull(maps_info$maps$errors) %>% paste0(collapse = "\n")
    stop(errors)

    # if no maps found
  } else if (maps_info$maps_count == 0) {

    message(glue::glue(
      "No maps found in workspace {glue::backtick(workspace)}.",
      "If this result is surprising, check the input in the `workspace` parameter.",
      .sep = "\n"
    ))

    # case 2: handle successfull maps request
  } else if (maps_info$maps_count > 0) {

    # page through maps; compile data
    maps <- purrr::map_dfr(
      .x = seq(from = 0, to = maps_info$maps_count, by = chunk_size),
      .f = ~ get_maps_by_chunk(
        workspace = workspace,
        take_n = chunk_size,
        skip_n = .x,
        nodes = nodes,
        server = server,
        user = user,
        password = password
      )
    )

    return(maps)

  }

}
