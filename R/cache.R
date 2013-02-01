Cache = function() {
  ee = new.env(parent = emptyenv())
  keys = function() ls(ee, all.names=TRUE)

  list(keys = keys,
       get = function(key) get(key, envir = ee),
       put = function(key, value) assign(key, value, envir = ee),
       remove = function(keys) rm(list = intersect(keys, keys()), envir = ee),
       clear = function() rm(list = keys(), envir = ee),
       has_key = function(key) key %in% keys())
}
