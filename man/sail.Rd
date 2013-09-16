\name{sail}
\alias{sail}
\title{Create a source abstraction interface layer (SAIL) object.}
\usage{
  sail(path = getwd(), extension = "R", use.cache = FALSE,
    simplify = TRUE)
}
\arguments{
  \item{path}{[\code{character(1)}]\cr Path to work in,
  will be created if it does not exists.}

  \item{extension}{[\code{character(1)}]\cr File extension
  to work with.}

  \item{use.cache}{[\code{logical(1)}]\cr Use a memory
  cache per global default.  Global option which can
  locally be overwritten in most functions.}
}
\value{
  Object of class \code{fail}. See Details.
}
\description{
  Create a source abstraction interface layer (SAIL)
  object.
}

