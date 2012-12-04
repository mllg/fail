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

checkString = function(x, na.ok=FALSE) {
  if (missing(x) || !is.character(x) || length(x) != 1L || (!na.ok && is.na(x)))
    stopf("Argument '%s' must be a character vector of length 1", deparse(substitute(x)))
}

checkStrings = function(x, min.len=1L, na.ok=FALSE) {
  if (missing(x) || !is.character(x) || length(x) < min.len || (!na.ok && any(is.na(x))))
    stopf("Argument '%s' must be a character vector of length >=%i", deparse(substitute(x)), min.len)
}

checkKeysFormat = function(keys) {
  ok = grepl("^\\.{0,1}[[:alpha:]_]{1}[[:alnum:]._]*$", keys)
  if (!all(ok))
    stopf("Illegal format for keys: '%s'", collapse(keys[!ok]))
}

checkKeysDuplicated = function(keys) {
  ok = anyDuplicated(keys)
  if (ok > 0L)
    stopf("Duplicated key '%s'", keys[ok])
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
  if(any(ns.missing)) {
    ns.sub = as.character(substitute(deparse(...)))[-1L]
    ns[ns.missing] = ns.sub[ns.missing]
  }
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
