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

checkKeysExist = function(keys, existing) {
  if (!all(existing))
    stopf("File for key '%s' not found", head(keys[!existing], 1L))
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

# allEqual = function(x, check.names = FALSE, tol = .Machine$double.eps^0.5) {
#   n.nas = sum(is.na(x))
#   if (n.nas > 0L && n.nas < length(x))
#     return(FALSE)
#
#   if (is.double(x) && tol > 0) {
#     equal = all(abs(head(x, 1L) - tail(x, -1L)) < tol)
#   } else if(is.logical(x)) {
#     equal = all(x)
#   } else {
#     equal == all(head(x, 1L) == tail(x, -1L))
#   }
#   equal && (!isTRUE(check.names) || allEqual(names(x), check.names=FALSE))
# }
#
simpleLoad = function(fn) {
  if (!file.exists(fn))
    return(NULL)
  ee = new.env(parent=emptyenv())
  ns = load(fn, envir=ee)
  if (length(ns) == 1L) {
    ee[[ns]]
  } else {
    as.list(ee)
  }
}

checkCollision = function(new, existing, overwrite) {
  found.sens = new %in% existing
  found.insens = !found.sens & new %in% tolower(existing)
  if (!overwrite && any(found.sens))
    stopf("File with key '%s' already pesent and overwrite is FALSE", head(new[found.sens], 1L))
  if (any(found.insens))
    warningf("Keys with same (case insensitve) name already present: '%s'", collapse(new[found.insens], ", "))
}
