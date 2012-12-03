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

#' @method as.list fail_list
#' @S3method as.list fail_list
as.list.fail = function(x) {
  x$as.list()
}
