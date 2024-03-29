% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assignments.R
\name{get_assignment_history}
\alias{get_assignment_history}
\title{Get assignment history}
\usage{
get_assignment_history(
  id,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)
}
\arguments{
\item{id}{Assignment ID number}

\item{server}{Full server web address (e.g., \code{https://demo.mysurvey.solutions}, \code{https://my.domain})}

\item{workspace}{Character. Name of the workspace whose assignments to get. In workspace list, value of \code{NAME}, not \verb{DISPLAY NAME}, for the target workspace.}

\item{user}{API user name}

\item{password}{API password}
}
\description{
Get the history of actions taken on the target assignment.
}
\details{
Wrapper for the \verb{GET ​/api​/v1​/assignments​/\{id\}​/history} endpoint
}
