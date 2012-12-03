#' @method names fail_list
#' @S3method names fail_list
names.fail_list = function(x) {
  x$ls()
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
