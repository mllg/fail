asFlag = function(x, default, na.ok = FALSE) {
  if (missing(x)) {
    if (!missing(default))
      return(default)
    stopf("Argument %s is missing", deparse(substitute(x)))
  }

  if (length(x) != 1L)
    stopf("Argument %s must have length 1", deparse(substitute(x)))

  if (is.logical(x)) {
    if (!na.ok && is.na(x))
      stopf("Argument %s may not be NA", deparse(substitute(x)))
    return(x)
  }

  x1 = try(as.logical(x), silent = TRUE)
  if (is.error(x1) || length(x1) != 1L || (!na.ok && is.na(x1)))
    stopf("Argument %s is not convertible to a logical value", deparse(substitute(x)))
  return(x1)
}

asKeys = function(keys, len, default) {
  if (missing(keys)) {
    if (!missing(default))
      return(default)
    stop("Keys are missing")
  }

  if (!is.character(keys)) {
    keys = try(as.character(keys))
    if (is.error(keys))
      stop("Keys must be of type character or be convertible to character")
  }

  if (!missing(len)) {
    if (length(keys) != len)
      stop("Keys must have length ", len)
  }

  if (any(is.na(keys)))
    stop("Keys contain NAs")

  # R variable pattern: "^((\\.[[:alpha:]._]+)|([[:alpha:]]+))[[:alnum:]_.]*$"
  pattern = "^[[:alnum:]._-]+$"
  ok = grepl(pattern, keys)
  if (! all(ok))
    stopf("Key '%s' in illegal format, see help", head(keys[!ok], 1L))

  return(keys)
}

checkPath = function(path) {
  assertString(path)
  if (file.exists(path)) {
    if (!isDirectory(path))
      stopf("Path '%s' is present but not a directory", path)
    if (.Platform$OS.type != "windows") {
      if (file.access(path, mode = 4L) != 0L)
        stopf("Path '%s' is not readable", path)
      if (file.access(path, mode = 2L) != 0L)
        stopf("Path '%s' is not writeable", path)
    }
  } else {
    if (!dir.create(path))
      stopf("Could not create directory '%s'", path)
  }
  return(path)
}

checkExtension = function(extension) {
  assertString(extension)
  if (grepl("[^[:alnum:]]", extension))
    stop("Extension contains illegal characters: ",
         collapse(strsplit(gsub("[[:alnum:]]", "", extension), ""), " "))
  return(extension)
}

checkCollision = function(keys) {
  dups = duplicated(tolower(keys))
  if (any(dups)) {
    warningf("The following keys result in colliding files on case insensitive file systems: %s",
             collapse(keys[dups]))
  }
  invisible(TRUE)
}

checkCollisionNew = function(new, old) {
  dups = new %nin% old & tolower(new) %in% tolower(old)
  if (any(dups))
    warningf("Keys collide on case insensitive file systems: %s", collapse(new[dups]))
  invisible(TRUE)
}

fn2key = function(.self, fn) {
  return(sub(sprintf("\\.%s$", .self$extension), "", fn))
}

key2fn = function(.self, key) {
  return(file.path(.self$path, sprintf("%s.%s", key, .self$extension)))
}

nkeys = function(.self) {
  length(list.files(.self$path, pattern = sprintf("\\.%s$", .self$extension), ignore.case=TRUE))
}
