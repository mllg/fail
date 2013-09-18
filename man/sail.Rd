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
  to work with.  Default is \dQuote{R}.}

  \item{use.cache}{[\code{logical(1)}]\cr Use a memory
  cache per global default.  Global option which can
  locally be overwritten in most functions.  Default is
  \code{FALSE}}

  \item{simplify}{[\code{character(1)}]\cr If only one
  object is defined in a sourced R file, should the return
  value be simplified? If set to \code{TRUE}, instead of a
  list containing one element the object itself will be
  returned instead.}
}
\value{
  Object of class \code{sail}. See the documentation of
  \code{\link{fail}} for details.
}
\description{
  This function returns an object of class \code{sail}
  which behaves like \code{\link{fail}}, but is indented
  for loading and saving R source code files.
}

