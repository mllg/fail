#' Create a source abstraction interface layer (SAIL) object.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to work in, will be created if it does not exists.
#' @param extension [\code{character(1)}]\cr
#'   File extension to work with.
#' @param use.cache [\code{logical(1)}]\cr
#'   Use a memory cache per global default.
#'   Global option which can locally be overwritten in most functions.
#' @return Object of class \code{fail}. See Details.
#' @export
sail = function(path = getwd(), extension = "R", use.cache = FALSE) {
  s = fail(path, extension, use.cache)
  ee = environment(s$ls)
  ee$.self$src = TRUE
  return(s)
}
