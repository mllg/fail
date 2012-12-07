as.flag = function(x) {
  if (missing(x))
    stopf("Argument %s is missing", deparse(substitute(x)))
  if (length(x) != 1L)
    stopf("Argument %s must have length 1", deparse(substitute(x)))
  if (is.logical(x)) {
    if (is.na(x))
      stopf("Argument %s may not be NA", deparse(substitute(x)))
    return(x)
  }
  conv = try(as.logical(x), silent=TRUE)
  if (is.error(conv) || length(conv) != 1L || is.na(conv))
    stopf("Argument %s is not convertible to a logical value", deparse(substitute(x)))
  return(conv)
}

assert.string = function(x, na.ok=FALSE) {
  if (missing(x))
    stopf("Argument '%s' is missing", deparse(substitute(x)))
  if (!is.character(x))
    stopf("Argument '%s' must be of type character", deparse(substitute(x)))
  if (length(x) != 1L)
    stopf("Argument '%s' must have length 1", deparse(substitute(x)))
  if (!na.ok && is.na(x))
    stopf("Arguments '%s' is NA", deparse(substitute(x)))
}

as.keys = function(keys, min.len, len, na.ok=FALSE, default) {
  if (missing(keys)) {
    if (missing(default))
      stop("Argument 'keys' is missing")
    return(default)
  }

  if (!is.character(keys)) {
    keys = try(as.character(keys))
    if (is.error(keys))
      stop("Argument 'keys' must be of type character or be convertible to character")
  }

  if (!missing(len)) {
    if (length(keys) != len)
      stop("Argument 'keys' must have length ", len)
  } else {
    if (!missing(min.len) && length(keys) <= min.len)
      stop("Argument 'keys' must have length >=", min.len)
  }

  if (!na.ok && any(is.na(keys)))
    stop("Arguments 'keys' contains NAs")

  # R variable pattern: "^((\\.[[:alpha:]._]+)|([[:alpha:]]+))[[:alnum:]_.]*$"
  pattern = "^[[:alnum:]._-]+$"
  ok = grepl(pattern, keys)
  if (! all(ok))
    stopf("Key '%s' in illeagal format, see help", head(keys[!ok], 1L))

  return(keys)
}

names2 = function(x) {
  ns = names(x)
  if (is.null(ns))
    return(rep(NA_character_, length(x)))
  replace(ns, ns == "", NA_character_)
}

argsAsNamedList = function(...) {
  args = list(...)
  ns = names2(args)
  ns.missing = is.na(ns)
  if (any(ns.missing)) {
    ns.sub = as.character(substitute(deparse(...)))[-1L]
    ns[ns.missing] = ns.sub[ns.missing]
  }
  ns[ns %in% c("NA", "NULL", "")] <- NA_character_
  setNames(args, ns)
}

simpleLoad = function(fn) {
  ee = new.env(parent=emptyenv())
  ns = load(fn, envir=ee)
  if (length(ns) == 1L) {
    ee[[ns]]
  } else {
    as.list(ee)
  }
}

simpleSave = function(fn, key, value) {
  ee = new.env(parent=emptyenv(), hash=FALSE)
  assign(key, value, envir=ee)
  save(list=key, envir=ee, file=fn)
  key
}

checkCollision = function(new, existing, overwrite) {
  found.sens = new %in% existing
  found.insens = !found.sens & new %in% tolower(existing)
  if (!overwrite && any(found.sens))
    stopf("File with key '%s' already pesent and overwrite is FALSE", head(new[found.sens], 1L))
  if (any(found.insens))
    warningf("Keys with same (case insensitve) name already present: '%s'", collapse(new[found.insens], ", "))
}
