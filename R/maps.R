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

    #Check if any user assigned
    any_user <- any(lapply(maps_df$users, length)>0)


    if (has_users & any_user) {

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
    } else if (has_users & any_user==FALSE) {
      map_list_df <- maps_df
      map_list_df['users'] <- NA_character_
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



#' Add user to Maps
#'
#' Add an interviewer account ('User') to a map uploaded to workspace
#'
#' GraphQL implementation of `addUserToMap(...)` HeadquartersMutation endpoint.
#'
#' @param mapfile Character. Full name of map file as uploaded to workspace and listed in [HQ-Maps](https://docs.mysurvey.solutions/headquarters/preloading/survey-setup-tab-import-copy-and-delete-questionnaire-templates-and-create-assignments/#maps). Include file extension ('.tpk', '.mmpk' or '.tiff')
#' @param mapuser Character. Full name of interviewer account to be added to \code{mapfile}
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace in which map is located. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API or admin user name for user that access to the workspace.
#' @param password Character. API or admin password
#'
#' @importFrom assertthat assert_that
#'
#' @return Data frame of map and users linked to it
#'
#' @export

add_user_to_map <- function(
  mapfile="",
  mapuser="",
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
) {


  #Validate parameters
  # Workspace:
  check_workspace_param(workspace = workspace)
  #Mapfile specified
  assertthat::assert_that(
    nchar(mapfile)>0,
    msg = paste0(
      "'mapfile' not specified."
    )
  )
  #mapuser specified
  assertthat::assert_that(
    nchar(mapuser)>0,
    msg = paste0(
      "'mapuser' not specified."
    )
  )




  #Build Base URL
  query_url <- httr::parse_url(server)
  #Add GraphQL path
  query_url$path <- "graphql"

  #Add Body: The GraphQL mutation. See https://github.com/arthur-shaw/susoapi/issues/28
  #For now, solely return fileName and users
  gql_body <-  stringr::str_squish((glue::glue('mutation {
        addUserToMap(
            workspace: <glue::double_quote(workspace)>,
            fileName: <glue::double_quote(mapfile)>
            userName: <glue::double_quote(mapuser)>
        )
        {
            fileName
            users {
              userName
            }
        }
        }', .open = "<", .close = ">")))

  #Post request
  addUserToMap.request <- httr::POST(
    url = query_url,
    httr::add_headers(
      Authorization = paste0(
        "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password))
      )
    ),
    body = list(query = gql_body),
    encode = "json"
  )

  #Store the response for subsequent analysis
  addUserToMap.response <- jsonlite::fromJSON(content(addUserToMap.request, as="text"), flatten = TRUE)

  ##Analysis
  #1) Errors
  if ("errors" %in% names(addUserToMap.response)) {
    # extract and display error(s)
    errors <- dplyr::pull(addUserToMap.response$errors) %>% paste0(collapse = "\n")
    message(glue::glue(
      "Attention: {errors}",
      "Empty data frame returned",
      .sep = "\n"
    ))
    #TODO: Is this actually desirable?
    result.df <- data.frame(
      map = NA_character_,
      users = NA_character_,
      updateDateUtc=NA_integer_
    )
    #2) Success
  } else {
    newusers <- paste(addUserToMap.response$data$addUserToMap$users$userName,collapse=",")
    message(glue::glue(
      "User {glue::backtick(mapuser)} successfully added to map {glue::backtick(mapfile)}.",
      .sep = "\n"
    ))

    #Return df
    result.df <- data.frame(
      map = addUserToMap.response$data$addUserToMap$fileName,
      users = newusers
    )
  }

  return(result.df)

}





#' Delete user from Map
#'
#' Removes an interviewer account ('User') from a map uploaded to workspace
#'
#' GraphQL implementation of `deleteUserFromMap(...)` HeadquartersMutation endpoint.
#'
#' @param mapfile Character. Full name of map file as uploaded to workspace and listed in [HQ-Maps](https://docs.mysurvey.solutions/headquarters/preloading/survey-setup-tab-import-copy-and-delete-questionnaire-templates-and-create-assignments/#maps). Include file extension ('.tpk', '.mmpk' or '.tiff')
#' @param mapuser Character. Full name of interviewer account to be deleted from \code{mapfile}
#' @param server Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})
#' @param workspace Character. Name of the workspace in which map is located. In workspace list, value of `NAME`, not `DISPLAY NAME`, for the target workspace.
#' @param user Character. API or admin user name for user that access to the workspace.
#' @param password Character. API or admin password
#'
#' @importFrom assertthat assert_that
#'
#' @return Data frame of map and users linked to it
#'
#' @export


delete_user_from_map <- function(
  mapfile="",
  mapuser="",
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
) {


  #Validate parameters
  # Workspace:
  check_workspace_param(workspace = workspace)
  #Mapfile specified
  assertthat::assert_that(
    nchar(mapfile)>0,
    msg = paste0(
      "'mapfile' not specified."
    )
  )
  #mapuser specified
  assertthat::assert_that(
    nchar(mapuser)>0,
    msg = paste0(
      "'mapuser' not specified."
    )
  )




  #Build Base URL
  query_url <- httr::parse_url(server)
  #Add GraphQL path
  query_url$path <- "graphql"

  #Add Body: The GraphQL mutation. See https://github.com/arthur-shaw/susoapi/issues/28
  #For now, solely return fileName and users
  gql_body <-  stringr::str_squish((glue::glue('mutation {
        deleteUserFromMap(
            workspace: <glue::double_quote(workspace)>,
            fileName: <glue::double_quote(mapfile)>
            userName: <glue::double_quote(mapuser)>
        )
        {
            fileName
            users {
              userName
            }
        }
        }', .open = "<", .close = ">")))

  #Post request
  deleteUserFromMap.request <- httr::POST(
    url = query_url,
    httr::add_headers(
      Authorization = paste0(
        "Basic ", jsonlite::base64_enc(input = paste0(user, ":", password))
      )
    ),
    body = list(query = gql_body),
    encode = "json"
  )

  #Store the response for subsequent analysis
  deleteUserFromMap.response <- jsonlite::fromJSON(content(deleteUserFromMap.request, as="text"), flatten = TRUE)

  ##Analysis
  #1) Errors
  if ("errors" %in% names(deleteUserFromMap.response)) {
    # extract and display error(s)
    errors <- dplyr::pull(deleteUserFromMap.response$errors) %>% paste0(collapse = "\n")
    message(glue::glue(
      "Attention: {errors}",
      "Empty data frame returned",
      .sep = "\n"
    ))
    #TODO: Is this actually desirable?
    result.df <- data.frame(
      map = NA_character_,
      users = NA_character_,
      updateDateUtc=NA_integer_
    )
    #2) Success
  } else {
    newusers <- paste(deleteUserFromMap.response$data$deleteUserFromMap$users$userName,collapse=",")
    message(glue::glue(
      "User {glue::backtick(mapuser)} successfully deleted from map {glue::backtick(mapfile)}.",
      "Updated list of users: {newusers} ",
      .sep = "\n"
    ))

    #TODO: Do we actually want to have df returned?
    result.df <- data.frame(
      map = deleteUserFromMap.response$data$deleteUserFromMap$fileName,
      users = newusers,
      #TODO: Decide if actually useful.
      updateDateUtc=as.POSIXlt(Sys.time(), tz = "UTC")
    )
  }

  return(result.df)

}

