#' @method print fail
#' @S3method print fail
print.fail = function(x, ...) {
  # FIXME re-add the information
  cat(sprintf("File Abstraction Interface layer"),
      sprintf("  %-9s : %i", "items", length(x$ls())),
      sprintf("  %-9s : %s", "methods", collapse(sub("\\.fail$", "", methods(class = "fail")), ", ")),
      sep = "\n")
}

#' @method as.list fail
#' @S3method as.list fail
as.list.fail = function(x, ...) {
  x$as.list(...)
}
