% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_alloc.R
\name{arrange_by_entity}
\alias{arrange_by_entity}
\title{Arrange a data.frame by entity}
\usage{
arrange_by_entity(data, entcol = "entity_id")
}
\arguments{
\item{data}{data containing a column for "entity_id".}
}
\value{
an arranged tibble.
}
\description{
Since entity_id is a character field, this function parsed the number and
arranges the data in increasing order by the entity id's number.
}
