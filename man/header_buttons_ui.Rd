% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_header_buttons.R
\name{header_buttons_ui}
\alias{header_buttons_ui}
\title{Header Buttons UI Module}
\usage{
header_buttons_ui(
  id,
  include_tour = TRUE,
  include_refresh = TRUE,
  include_help = TRUE,
  include_disclaimer = TRUE,
  include_contact = TRUE,
  include_logout = TRUE,
  contacts = NULL
)
}
\arguments{
\item{id}{namespace ID}

\item{include_tour}{logical - include a 'tour app' button?}

\item{include_refresh}{logical - include a 'refresh' button?}

\item{include_help}{logical - include a 'help' button?}

\item{include_disclaimer}{logical - include a 'disclaimer' button?}

\item{include_contact}{logical - include a 'contact' button?}

\item{include_logout}{logical - include a 'logout' button? Note this will replace the
default shiny logout.}

\item{contacts}{contacts list from \code{\link[=contacts]{contacts()}}.}
}
\value{
\code{htmltools::tagList()}
}
\description{
Header Buttons UI Module
}
