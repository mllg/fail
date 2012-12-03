#' @method print fal
#' @S3method print fal
print.fal = function(x, ...) {
  opts = environment(x$list)$.opts
  cat(sprintf("Key-value store on directory '%s':", opts$path),
      sprintf("  %-9s : %i", "items", length(x$list())),
      sprintf("  %-9s : %i", "cached", length(x$cached())),
      sprintf("  %-9s : .%s", "extension", opts$extension),
      sprintf("  %-9s : %s", "cache", opts$cache),
      sprintf("  %-9s : %s", "overwrite", opts$overwrite),
      sprintf("  %-9s : %s", "functions", collapse(sort(names(x)), ", ")),
      sprintf("  %-9s : %s", "methods", collapse(sub("\\.fal$", "", methods(class="fal")), ", ")),
      sep = "\n")
}

#' @method names fal
#' @S3method names fal
names.fal = function(x) {
  x$list()
}

#' @method as.list fal
#' @S3method as.list fal
as.list.fal = function(x, keys) {
  x$as.list(keys)
}

#' @method [[ fal
#' @S3method [[ fal
`[[.fal` = function(x, key) {
  x$get(key)
}

#' @method [[<- fal
#' @S3method [[<- fal
`[[<-.fal` = function(x, key, value) {
  if (length(key) > 1L)
    stopf("subscript out of bounds")
  x$put(li = setNames(list(value), key))
  invisible(x)
}

#' @method [ fal
#' @S3method [ fal
`[.fal` = function(x, keys) {
  x$as.list(keys)
}

#' @method [<- fal
#' @S3method [<- fal
`[<-.fal` = function(x, keys, value) {
  if (length(keys) != length(value))
    stop("Length mismatch")
  x$put(li = as.list(setNames(value, keys)))
  invisible(x)
}
