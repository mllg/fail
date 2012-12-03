#' @method print fal
#' @S3method print fal
print.fal = function(x, ...) {
  opts = environment(x$ls)$.opts
  cat(sprintf("Key-value store on directory '%s':", opts$path),
      sprintf("  %-9s : %i", "items", length(x$ls())),
      sprintf("  %-9s : %i", "cached", length(x$cached())),
      sprintf("  %-9s : .%s", "extension", opts$extension),
      sprintf("  %-9s : %s", "cache", opts$cache),
      sprintf("  %-9s : %s", "overwrite", opts$overwrite),
      sprintf("  %-9s : %s", "functions", collapse(x$funs(), ", ")),
      sprintf("  %-9s : %s", "methods", collapse(sub("\\.fal$", "", methods(class="fal")), ", ")), #FIXME class rigth?
      sep = "\n")
}

#' @method names fal_list
#' @S3method names fal_list
names.fal = function(x) {
  x$ls()
}

#' @method as.list fal_list
#' @S3method as.list fal_list
as.list.fal = function(x, ...) {
  x$as.list(...)
}

#' @method [[ fal_list
#' @S3method [[ fal_list
`[[.fal` = function(x, key) {
  # FIXME should return NULL if key does not exists
  x$get(key)
}

#' @method [[<- fal_list
#' @S3method [[<- fal_list
`[[<-.fal` = function(x, key, value) {
  if (length(key) > 1L)
    stopf("subscript out of bounds")
  if (is.null(value))
    x$remove(key)
  else
    x$put(li = setNames(list(value), key))
  invisible(x)
}

#' @method [ fal_list
#' @S3method [ fal_list
`[.fal` = function(x, keys) {
  # FIXME should return NULL if key does not exists
  x$as.list(keys)
}

#' @method [<- fal_list
#' @S3method [<- fal_list
`[<-.fal` = function(x, keys, value) {
  if (length(keys) != length(value))
    stop("Length mismatch")
  x$put(li = as.list(setNames(value, keys)))
  invisible(x)
}
