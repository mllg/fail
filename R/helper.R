as.flag = function(x, default) {
  if (missing(x)) {
    if (!missing(default))
      return(default)
    stopf("Argument %s is missing", deparse(substitute(x)))
  }

  if (length(x) != 1L)
    stopf("Argument %s must have length 1", deparse(substitute(x)))

  if (is.logical(x)) {
    if (is.na(x))
      stopf("Argument %s may not be NA", deparse(substitute(x)))
    return(x)
  }

  x1 = try(as.logical(x), silent = TRUE)
  if (is.error(x1) || length(x1) != 1L || is.na(x1))
    stopf("Argument %s is not convertible to a logical value", deparse(substitute(x)))
  return(x1)
}

assert.string = function(x, na.ok = FALSE) {
  if (missing(x))
    stopf("Argument '%s' is missing", deparse(substitute(x)))
  if (!is.character(x))
    stopf("Argument '%s' must be of type character", deparse(substitute(x)))
  if (length(x) != 1L)
    stopf("Argument '%s' must have length 1", deparse(substitute(x)))
  if (!na.ok && is.na(x))
    stopf("Arguments '%s' is NA", deparse(substitute(x)))
}

as.keys = function(keys, len, default) {
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

names2 = function(x) {
  ns = names(x)
  if (is.null(ns))
    return(rep(NA_character_, length(x)))
  return(replace(ns, ns == "", NA_character_))
}

argsAsNamedList = function(...) {
  args = list(...)
  ns = names2(args)
  ns.missing = is.na(ns)
  if (any(ns.missing)) {
    ns.sub = as.character(substitute(deparse(...)))[-1L]
    ns[ns.missing] = ns.sub[ns.missing]
  }
  return(setNames(args, replace(ns, ns %in% c("NA", "NULL", ""), NA_character_)))
}

simpleLoad = function(fn) {
  ee = new.env(parent = emptyenv(), hash = FALSE)
  ns = load(fn, envir = ee)
  if (length(ns) == 1L)
    return(ee[[ns]])
  return(as.list(ee))
}

simpleSave = function(fn, key, value) {
  ee = new.env(parent = emptyenv(), hash = FALSE)
  assign(key, value, envir = ee)
  save(list = key, envir = ee, file = fn)
  return(key)
}

checkPath = function(path) {
  assert.string(path)
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
  return(path)
}

checkExtension = function(extension) {
  assert.string(extension)
  if (grepl("[^[:alnum:]]", extension))
    stop("Extension contains illegal characters: ",
         collapse(strsplit(gsub("[[:alnum:]]", "", extension), ""), " "))
  return(extension)
}

checkCollision = function(keys) {
  dups = duplicated(tolower(keys))
  if (any(dups)) {
    warningf("The following keys would result in colliding files on case insensitive file systems: %s",
             collapse(keys[dups]))
  }
}

checkCollisionNew = function(new, old) {
  dups = new %nin% old & tolower(new) %in% tolower(old)
  if (any(dups))
    warningf("Keys would collide on case insensitive file systems: %s", collapse(new[dups]))
}

fn2key = function(self, fn) {
  return(sub(sprintf("\\.%s$", self$extension), "", fn))
}

key2fn = function(self, key) {
  return(file.path(self$path, sprintf("%s.%s", key, self$extension)))
}
