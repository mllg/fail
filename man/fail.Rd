\name{fail}
\alias{fail}
\title{Create a file abstraction interface layer (FAIL).}
\usage{
  fail(path = getwd(), extension = "RData", cache = FALSE,
    overwrite = TRUE)
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
}
\value{
  Object of class \code{fail}. See Details.
}
\description{
  This is the constructor of a fail object which provides
  functions as described in the details.
}
\details{
  An object with the following functions is returned:
  \describe{ \item{\code{ls(pattern=NULL)}}{ Function to
  list keys in directory \code{path}.  }
  \item{\code{get(key, cache)}}{ Function to load a file
  identified by \code{key} from directory \code{path}.
  Argument \code{cache} can be set to temporarily overwrite
  the global \code{cache} flag.  } }
}

