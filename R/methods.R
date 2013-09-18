printObject = function(x, type) {
  info = x$info()
  cat(sprintf("%s on path %s", type, info$path),
      sprintf("  %-9s : %s", "extension", info$extension),
      sprintf("  %-9s : %s", "use.cache", info$use.cache),
      sprintf("  %-9s : %s", "simplify", info$simplify),
      sprintf("  %-9s : %i", "items", length(x$ls())),
      sprintf("  %-9s : %i", "cached", length(x$cached())),
      sprintf("  %-9s : %s", "functions", collapse(names(x))),
      sprintf("  %-9s : %s", "methods", collapse(sub("\\.fail$", "", methods(class = "fail")), ", ")),
      sep = "\n")
}

#' @method print fail
#' @S3method print fail
print.fail = function(x, ...) {
  printObject(x, "File Abstraction Interface Layer")
}

#' @method print sail
#' @S3method print sail
print.sail = function(x, ...) {
  printObject(x, "Source Abstraction Interface Layer")
}

#' @method as.list fail
#' @S3method as.list fail
as.list.fail = function(x, ...) {
  x$as.list(...)
}

#' @method as.list sail
#' @S3method as.list sail
as.list.sail = function(x, ...) {
  x$as.list(...)
}
