% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/workspaces.R
\name{disable_workspace}
\alias{disable_workspace}
\title{Disable a workspace}
\usage{
disable_workspace(
  name,
  server = Sys.getenv("SUSO_SERVER"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)
}
\arguments{
\item{name}{Character. Name identifier of workspace to disable.}

\item{server}{Character. Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})}

\item{user}{Character. Admin user name}

\item{password}{Character. Admin password}
}
\value{
Server-side side-effect of disabling a workspace.
}
\description{
Disable the workspace with name ID \code{name}.
}
\details{
Wrapper for \verb{POST ​/api​/v1​/workspaces​/\{name\}​/disable} endpoint.
}
