#' @method print fail
#' @S3method print fail
print.fail = function(x, ...) {
  opts = environment(x$ls)$.opts
  cat(sprintf("File Abstraction Interface layer on directory '%s':", opts$path),
      sprintf("  %-9s : %i", "items", length(x$ls())),
      sprintf("  %-9s : %i", "cached", length(x$cached())),
      sprintf("  %-9s : .%s", "extension", opts$extension),
      sprintf("  %-9s : %s", "cache", opts$cache),
      sprintf("  %-9s : %s", "overwrite", opts$overwrite),
      sprintf("  %-9s : %s", "functions", collapse(x$funs(), ", ")),
      sprintf("  %-9s : %s", "methods", collapse(sub("\\.fail$", "", methods(class=class(x)[1L])), ", ")),
      sep = "\n")
}

#' @method names fail_list
#' @S3method names fail_list
names.fail_list = function(x) {
  #FIXME add pattern?
  x$ls()
}

#' @method as.list fail_list
#' @S3method as.list fail_list
as.list.fail_list = function(x, ...) {
  x$as.list(...)
}

#' @method [[ fail_list
#' @S3method [[ fail_list
`[[.fail_list` = function(x, key) {
  x$get(key)
}

#' @method [[<- fail_list
#' @S3method [[<- fail_list
`[[<-.fail_list` = function(x, key, value) {
  # let R's list operator handle all the crazy rules
  li = list()
  li[[key]] = value

  if (is.null(value)) {
    x$remove(key)
  } else {
    x$put(li = li)
  }
  invisible(x)
}

#' @method [ fail_list
#' @S3method [ fail_list
`[.fail_list` = function(x, keys) {
  x$as.list(keys)
}

#' @method [<- fail_list
#' @S3method [<- fail_list
`[<-.fail_list` = function(x, keys, value) {
  if (length(keys) == 0L)
    keys = x$ls()
  # let R's list operator handle all the crazy rules
  li = list()
  li[keys] <- value

  # remove those which were NULL and put the rest
  x$remove(setdiff(keys, names(li)))
  x$put(li = li)

  invisible(x)
}
