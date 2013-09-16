makeObject = function(.self) {
  force(.self)
  list(
    ls = function(pattern = NULL) {
      Ls(.self, pattern)
    },
    get = function(key, use.cache) {
      Get(.self, as.keys(key, len = 1L),
          use.cache = as.flag(use.cache, default = .self$use.cache))
    },
    put = function(..., keys, li = list(), use.cache) {
      Put(.self, ..., keys = keys, li = as.list(li),
          use.cache = as.flag(use.cache, default = .self$use.cache))
    },
    remove = function(keys) {
      Remove(.self, as.keys(keys))
    },
    as.list = function(keys, use.cache) {
      AsList(.self, as.keys(keys, default = Ls(.self)),
             use.cache = as.flag(use.cache, default = .self$use.cache))
    },
    apply = function(FUN, ..., keys, use.cache, simplify = FALSE, use.names = TRUE) {
      Apply(.self, FUN, ..., keys = as.keys(keys, default = Ls(.self)),
            use.cache = as.flag(use.cache, default = .self$use.cache),
            simplify = as.flag(simplify), use.names = as.flag(use.names))
    },
    mapply = function(FUN, ..., keys, use.cache, moreArgs = NULL, simplify = FALSE, use.names = TRUE) {
      Mapply(.self, FUN, ..., keys = as.keys(keys, default = Ls(.self)),
             use.cache = as.flag(use.cache, default = .self$use.cache),
             moreArgs = as.list(moreArgs), simplify = as.flag(simplify), use.names = as.flag(use.names))
    },
    assign = function(keys, envir = parent.frame(), use.cache) {
      Assign(.self, keys = as.keys(keys, default = Ls(.self)), envir = as.environment(envir),
             use.cache = as.flag(use.cache, default = .self$use.cache))
    },
    size = function(keys, unit = "b") {
      match.arg(unit, choices = names(UNITCONVERT))
      Size(.self, as.keys(keys, default = Ls(.self)), unit = unit)
    },
    clear = function(keys) {
      Clear(.self, as.keys(keys, default = Ls(.self)))
    },
    cached = function() {
      Cached(.self)
    },
    info = function() {
      Info(.self)
    }
  )
}
