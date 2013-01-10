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
fail = function(path=getwd(), extension = "RData", cache = FALSE, overwrite = TRUE) {
  # Internal functions frequently used, w/o argument checks
  Ls = function(pattern = NULL) {
    keys = fn2key(.opts, list.files(.opts$path, pattern = sprintf("\\.%s$", .opts$extension)))
    if (!is.null(pattern))
      keys = keys[grepl(pattern, keys)]
    keys
  }

  Get = function(key, cache = .opts$cache) {
    fn = key2fn(.opts, key)
    if (!file.exists(fn))
      stopf("File for key '%s' (%s) not found", key, fn)
    if (!cache)
      return(simpleLoad(fn))
    if (key %nin% .cache$keys())
      .cache$put(key, simpleLoad(fn))
    .cache$get(key)
  }

  Put = function(x, cache = .opts$cache) {
    if (!length(x))
      return(invisible(character(0L)))
    keys = names(x)

    if (cache)
      mapply(.cache$put, key = keys, value = x, USE.NAMES = FALSE, SIMPLIFY = FALSE)

    invisible(mapply(simpleSave, fn = key2fn(.opts, keys), key = keys, value = x, USE.NAMES = FALSE))
  }

  # Argument checking and initilization
  assert.string(path)
  assert.string(extension)

  if (file.exists(path)) {
    if (!isDirectory(path))
      stopf("Path '%s' is present but not a directory", path)
    if (!grepl("windows", Sys.info()["sysname"], ignore.case = TRUE)) {
      if (file.access(path, mode = 4L) != 0L)
        stopf("Path '%s' is not readable", path)
      if (file.access(path, mode = 2L) != 0L)
        stopf("Path '%s' is not writeable", path)
    }
  } else {
    if (!dir.create(path))
      stopf("Could not create directory '%s'", path)
  }

  if (grepl("[^[:alnum:]]", extension))
    stop("Extension contains illegal characters: ",
         collapse(strsplit(gsub("[[:alnum:]]", "", extension), ""), " "))

  # set up list of options and remove obsolete vars
  .opts = list(path = normalizePath(path), extension = extension, cache = as.flag(cache), overwrite = as.flag(overwrite))
  rm(list = names(.opts))

  # initialize cache
  .cache = Cache()

  # quick sanity check
  if (anyDuplicated(tolower(Ls())))
    warningf("The following files would collide on case insensitive file systems: %s",
             collapse(basename(key2fn(.opts, Ls())), sep = ", "))

  setClasses(list(
    ls = function(pattern = NULL) {
      if (!is.null(pattern))
        assert.string(pattern)
      Ls(pattern)
    },

    get = function(key, cache = .opts$cache) {
      Get(as.keys(key, len = 1L), as.flag(cache))
    },

    put = function(..., li = list(), keys, overwrite = .opts$overwrite, cache = .opts$cache) {
      if (!is.list(li))
        stop("Argument 'li' must be a list")
      args = argsAsNamedList(...)
      nargs = length(args) + length(li)

      if (!missing(keys)) {
        keys = as.keys(keys, len = nargs)
        names(args) = head(keys, length(args))
        names(li) = tail(keys, length(li))
      } else {
        keys.args = names2(args)
        if (any(is.na(keys.args)))
          stop("Could not determine all names from '...' arguments. Consider using key=value syntax")

        keys.li = names2(li)
        if (any(is.na(keys.li)))
          stop("Argument 'li' must be a fully named list")

        keys = as.keys(c(keys.args, keys.li))
      }

      if (anyDuplicated(keys))
        stopf("Argument 'keys' contains duplicated name: %s", head(keys[duplicated(keys)], 1L))

      checkCollision(keys, Ls(), as.flag(overwrite))

      cache = as.flag(cache)
      invisible(c(Put(args, cache = cache), Put(li, cache = cache)))
    },

    remove = function(keys) {
      keys = unique(as.keys(keys))
      fns = key2fn(.opts, keys)

      ok = file.exists(fns)
      if (!all(ok))
        stopf("Files not found for keys: %s", collapse(keys[!ok]))

      ok = file.remove(fns)
      if (!all(ok))
        warningf("Files could not be removed: %s", collapse(fns[!ok], ", "))

      .cache$remove(keys[ok])

      invisible(setNames(ok, keys))
    },

    apply = function(FUN, ..., keys, cache = .opts$cache, simplify = FALSE, use.names = TRUE) {
      FUN = match.fun(FUN)
      keys = as.keys(keys, default = Ls())

      wrapper = function(key, cache, ...) {
        res = try(FUN(Get(key, cache = cache), ...), silent = TRUE)
        if (is.error(res))
          stopf("Error applying function on key '%s': %s", key, as.character(res))
        res
      }

      sapply(keys, wrapper, cache = as.flag(cache), ..., USE.NAMES = as.flag(use.names), simplify = as.flag(simplify))
    },

    size = function(keys, unit = "b") {
      keys = as.keys(keys, default = Ls())
      conv = setNames(c(1L, 1024L, 1048576L, 1073741824L), c("b", "kB", "Mb", "Gb"))
      match.arg(unit, choices = names(conv))

      size = as.integer(file.info(key2fn(.opts, keys))$size)
      setNames(size / conv[unit], keys)
    },

    as.list = function(keys, cache = .opts$cache) {
      keys = as.keys(keys, default = Ls())
      setNames(lapply(keys, Get, cache = as.flag(cache)), keys)
    },

    assign = function(keys, envir = parent.frame(), cache = .opts$cache) {
      keys = as.keys(keys, default = Ls())
      cache = as.flag(cache)
      force(envir)

      lapply(keys, function(key, ee) assign(key, Get(key, cache = cache), envir = ee), ee = envir)
      invisible(TRUE)
    },

    clear = function(keys) {
      if (missing(keys)) {
        .cache$clear()
      } else {
        .cache$remove(as.keys(keys))
      }
      invisible(TRUE)
    },

    cached = function() {
      .cache$keys()
    }), "fail")
}
