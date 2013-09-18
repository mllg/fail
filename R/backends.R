loadRData = function(fn, simplify = TRUE) {
  ee = new.env(parent = emptyenv(), hash = FALSE)
  ns = load(fn, envir = ee)
  if (simplify && length(ns) == 1L)
    return(ee[[ns]])
  return(as.list(ee))
}

saveRData = function(fn, key, value) {
  ee = new.env(parent = emptyenv(), hash = FALSE)
  assign(key, value, envir = ee)
  save(list = key, envir = ee, file = fn)
  return(invisible(key))
}

loadR = function(fn, simplify = FALSE) {
  ee = new.env(parent = .GlobalEnv, hash = FALSE)
  sys.source(fn, ee)
  ns = ls(ee, all.names = TRUE)
  if (simplify && length(ns) == 1L)
    return(ee[[ns]])
  return(as.list(ee))
}

saveR = function(fn, key, value) {
  ee = new.env(parent = emptyenv(), hash = FALSE)
  assign(key, value, envir = ee)
  dump(key, file = fn, envir = ee)
}
