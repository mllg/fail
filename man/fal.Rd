\name{fal}
\alias{fal}
\title{Create a file abstraction layer.}
\usage{
  fal(path = getwd(), extension = "RData", cache = FALSE,
    overwrite = TRUE, list.interface = TRUE)
}
\arguments{
  \item{path}{[\code{character(1)}]\cr Path to work in,
  will be created if it does not exists.}

  \item{extension}{[\code{character(1)}]\cr File extension
  to work with.}

  \item{cache}{[\code{logical(1)}]\cr Use a memory cache as
  global default.  Global option which can locally be
  overwritten in most functions.}

  \item{overwrite}{[\code{logical(1)}]\cr Protect files
  from being accidently overwritten.  Global option which
  can locally be overwritten in most functions.}

  \item{list.interface}{[\code{logical(1)}]\cr Enable S3
  methods to enable an interface similar to lists?  Default
  is \code{TRUE}.}
}
\value{
  Object of class \code{fal}. See Details.
}
\description{
  Create a file abstraction layer.
}

