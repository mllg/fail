#' Create a source abstraction interface layer (SAIL) object.
#'
#' This function returns an object of class \code{sail} which behaves
#' like \code{\link{fail}}, but is indented for loading and saving
#' R source code files.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to work in, will be created if it does not exists.
#' @param extension [\code{character(1)}]\cr
#'   File extension to work with.
#'   Default is \dQuote{R}.
#' @param use.cache [\code{logical(1)}]\cr
#'   Use a memory cache per global default.
#'   Global option which can locally be overwritten in most functions.
#'   Default is \code{FALSE}
#' @param simplify [\code{character(1)}]\cr
#'   If only one object is defined in a sourced R file,
#'   should the return value be simplified? If set to \code{TRUE},
#'   instead of a list containing one element the object itself will be returned.
#' @return Object of class \code{sail}. See the documentation of \code{\link{fail}}
#'   for details.
#' @export
sail = function(path = getwd(), extension = "R", use.cache = FALSE, simplify = TRUE) {
  .self = list(path = checkPath(path),
               extension = checkExtension(extension),
               use.cache = as.flag(use.cache),
               simplify = as.flag(simplify, na.ok = TRUE),
               cache = Cache(),
               loadFun = loadR,
               saveFun = saveR
               )
  checkCollision(Ls(.self))
  setClasses(makeObject(.self), "sail")
}
