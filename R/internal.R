icache = function() {
  ee = new.env(parent = emptyenv())
  keys = function() ls(ee, all.names=TRUE)

  list(keys = keys,
       get = function(key) get(key, envir = ee),
       put = function(key, value) assign(key, value, envir = ee),
       remove = function(keys) rm(list = intersect(keys, keys()), envir = ee),
       clear = function() rm(list = keys(), envir = ee),
       has_key = function(key) key %in% keys())
}

ifail = function(path, extension) {
  force(path)
  force(extension)
  .cache = icache()

  fn2key = function(fn) sub(sprintf("\\.%s$", extension), "", fn)
  key2fn = function(key) file.path(path, sprintf("%s.%s", key, extension))

  list(
    ls = function() {
      fn2key(list.files(.opts$path, pattern = sprintf("\\.%s$", .opts$extension)))
    },

    get = function(key, use.cache = FALSE) {
      if (use.cache && .cache$has_key(key))
        return(.cache$get(key))

      fn = key2fn(key)
      if (!file.exists(fn))
        stopf("File for key '%s' (%s) not found", key, fn)
      simpleLoad(fn)
    },

    put = function(key, value, use.cache = FALSE) {
      if (use.cache)
        .cache$put(key, value)
      simpleSave(key2fn(key), key, value)
    },

    rm = function(key) {
      .cache$remove(key)
      fn = key2fn(key)
      file.exists(fn) && file.remove(fn)
    },

    cache = function() {
      .cache
    },

    size = function(keys) {
      as.integer(file.info(key2fn(keys))$size)
    }
  )
}
