% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces.R
\name{delete_workspace}
\alias{delete_workspace}
\title{Delete a workspace}
\usage{
delete_workspace(
  name,
  server = Sys.getenv("SUSO_SERVER"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)
}
\arguments{
\item{name}{Character. Name identifier of workspace to delete.}

\item{server}{Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})}

\item{user}{Character. Admin or API user name}

\item{password}{Character. Admin or API password}
}
\value{
Server-side side-effect of deleting a workspace.
}
\description{
Deletes workspace whose name matches \code{name}.
}
\details{
Wrapper for \verb{DELETE ​/api​/v1​/workspaces​/\{name\}} endpoint.
}
