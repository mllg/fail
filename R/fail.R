#' Create a file abstraction interface layer (FAIL).
#'
#' This is the constructor of a fail object which provides functions as described in the details.
#' The general idea is to not bother about file path joining and file extensions.
#' Instead FAIL offers a key-value like interface to RData files in a specified directory.
#' The filename (without extension) acts as the key while the object inside the RData file is the value.
#' Files can be refered to using just the keys.
#' FAIL offers besides some utilitiy functions implementations for the basic operations \dQuote{list},
#' \dQuote{load}, \dQuote{save}, \dQuote{remove} and \dQuote{apply}.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to work in, will be created if it does not exists.
#' @param extension [\code{character(1)}]\cr
#'   File extension to work with.
#' @param cache [\code{logical(1)}]\cr
#'   Use a memory cache as global default.
#'   Global option which can locally be overwritten in most functions.
#' @param overwrite [\code{logical(1)}]\cr
#'   Protect files from being accidently overwritten.
#'   Global option which can locally be overwritten in most functions.
#' @return Object of class \code{fail}. See Details.
#' @details
#'   For a quick introduction on the usage, see \url{https://github.com/mllg/fail}.
#'
#'   An object with the following functions is returned by \code{fail}:
#'   \describe{
#'     \item{\code{ls(pattern=NULL)}}{
#'       Function to list keys in directory \code{path} matching a regular expression pattern \code{pattern}.
#'     }
#'     \item{\code{get(key, cache)}}{
#'       Function to load a file identified by \code{key} from directory \code{path}.
#'       Argument \code{cache} can be set to temporarily overwrite the global \code{cache} flag.
#'     }
#'     \item{\code{put(..., li, keys, overwrite, cache)}}{
#'       Function to save objects to to directory \code{path}.
#'       Names for objects provided via \code{...} will be looked up or can be provided using a \code{key = value} syntax.
#'       More objects can be passed as a named list using the argument \code{li}: Each list item will be saved to a separate file.
#'       If you provide \code{keys} as a character vector, these keys will be taken instead.
#'       The vector than must be of length \code{length(...) + length(li)}. The first keys will be used to name objects in \code{...},
#'       the remaining to name objects in \code{li}.
#'       Arguments \code{overwrite} and \code{cache} temporarily overwrite the global \code{overwrite} or \code{cache} flags, respectively.
#'     }
#'     \item{\code{remove(keys)}}{
#'       Function to remove files identified by \code{keys} from directory \code{path}.
#'     }
#'     \item{\code{apply(FUN, ..., keys, cache, simplify=FALSE, use.names=TRUE)}}{
#'       Apply a function \code{FUN} on files identified by \code{keys}. The loaded R objects will be past unnamed as first argument.
#'       Use \code{...} for additional function arguments.
#'       Argument \code{cache} can be set to temporarily overwrite the global \code{cache} flag.
#'       For arguments \code{simplify} and \code{use.names}, see \code{\link{lapply}}.
#'       Keys will be used to name the (possibly simplified) returned list.
#'    }
#'    \item{\code{as.list(keys, cache)}}{
#'       Return a named list of \code{keys}. \code{keys} defaults to all keys available.
#'       Argument \code{cache} can be set to temporarily overwrite the global \code{cache} flag.
#'    }
#'    \item{\code{assign(keys, envir=parent.frame(), cache)}}{
#'       Assigns all objects listed in \code{keys} in the environment \code{envir}.
#'       Argument \code{cache} can be set to temporarily overwrite the global \code{cache} flag.
#'    }
#'    \item{\code{clear(keys)}}{
#'       Clear the cache to free memory. \code{keys} defaults to all keys available.
#'    }
#'    \item{\code{cached()}}{
#'       Returns a character vector of keys of cached objects.
#'    }
#'    \item{\code{size(keys, unit="b")}}{
#'       Get the file size in Bytes of the filey identified by \code{keys}. \code{keys} defaults to all keys available.
#'       Argument \code{unit} accepts \dQuote{b}, \dQuote{Kb}, \dQuote{Mb} and \dQuote{Gb} and can be used to convert Bytes to KiloBytes, MegaBytes or GigaBytes, respectively.
#'    }
#'   }
#'   Furthermore the package provides S3 methods for \code{\link{print}} and \code{\link{as.list}}.
#'
#'   Be aware of the following restriction regarding file names and keys:
#'   The package performs some basic checks for illegal characters on the key names.
#'   In principle all characters matching the pattern \dQuote{[a-zA-Z0-9._-]} are allowed and should work on most or all file systems.
#'   But be careful with key names which are not compatible with R's variable naming restrictions, e.g. using the minus character: these can have unwanted side effects.
#'
#'   If two files would collide on case-insensitive file systems like Windows' NTFS, the package will throw some  warnings.
#'
#' @export
#' @examples
#' # initialize a FAIL in a temporary directory
#' files <- fail(tempfile(""))
#'
#' # save x and y, vectors of random numbers
#' x <- runif(100)
#' files$put(x, y = runif(100))
#'
#' # save columns of the iris data set as separate files
#' files$put(li = as.list(iris))
#'
#' # load an object from the file system
#' files$get("Species")
#' files$as.list(c("x", "y"))
#'
#' # remove an object (and related file)
#' files$remove("Species")
#'
#' # apply a function over files
#' files$apply(mean)
#'
#' # show file size informations
#' files$size()
#'
#' # get an object and cache it
#' files$get("x", cache = TRUE)
#' files$cached()
#' files$clear()
#' files$cached()
fail = function(path = getwd(), extension = "RData", use.cache = FALSE) {
  ### argument checks
  checkPath(path)
  checkExtension(extension)
  .use.cache = as.flag(use.cache)
  .fail = ifail(path, extension)
  checkCollision(.fail$ls())

  ### clean up
  rm(path, extension, use.cache)

  setClasses(list(
    ls = function(pattern = NULL) {
      keys = .fail$ls()
      if (!is.null(pattern))
        keys = keys[grepl(pattern, keys)]
      keys
    },

    get = function(key, use.cache = .use.cache) {
      .fail$get(as.keys(key, len = 1L), as.flag(use.cache))
    },

    as.list = function(keys, use.cache = .use.cache) {
      keys = as.keys(keys, default = .fail$ls())
      setNames(lapply(keys, .fail$get, use.cache = as.flag(use.cache)), keys)
    },

    put = function(..., li = list(), use.cache = .use.cache) {
      args = c(argsAsNamedList(...), as.list(li))
      keys = names2(args)
      if (any(is.na(keys)))
        stop("Could not determine all key names from input")
      if (anyDuplicated(keys) > 0L)
        stop("Duplicated key names")

      mapply(.fail$put, key = keys, value = args, MoreArgs = list(use.cache = as.flag(use.cache)), USE.NAMES=FALSE, SIMPLIFY=FALSE)
      keys
    },

    remove = function(keys) {
      ok = vapply(as.keys(keys), .fail$rm, TRUE)
      if (!all(ok))
        warning("Some files could not be removed")
      ok
    },

    apply = function(FUN, ..., keys, use.cache = .use.cache, simplify = FALSE, use.names = TRUE) {
      FUN = match.fun(FUN)

      wrapper = function(key, .use.cache, ...) {
        res = try(FUN(.fail$get(key, use.cache = .use.cache), ...), silent = TRUE)
        if (is.error(res))
          stopf("Error applying function on key '%s': %s", key, as.character(res))
        res
      }

      sapply(as.keys(keys, default = .fail$ls()), wrapper, .use.cache = as.flag(.use.cache), ...,
             USE.NAMES = as.flag(use.names), simplify = as.flag(simplify))
    },

    assign = function(keys, envir = parent.frame(), use.cache = .use.cache) {
      lapply(as.keys(keys, default = .fail$ls()),
             function(key, envir) assign(key, .fail$get(key, use.cache = use.cache), envir = envir), envir = envir)
      invisible(TRUE)
    },

    size = function(keys, unit = "b") {
      keys = as.keys(keys, default = .fail$ls())
      conv = setNames(c(1L, 1024L, 1048576L, 1073741824L), c("b", "kB", "Mb", "Gb"))
      match.arg(unit, choices = names(conv))
      setNames(.fail$size(keys) / conv[unit], keys)
    },

    clear = function(keys) {
      keys = as.keys(keys, default = .fail$ls())
      .fail$cache()$remove(keys)
      invisible(TRUE)
    },

    cached = function() {
      .fail$cache()$keys()
    }
  ), "fail")
}
