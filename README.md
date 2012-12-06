# fail

File Abstraction Interface Layer (FAIL) for R mimicking a key-value store.

This package simplifies working with RData files managed in directories.
A FAIL is constructed on a single directory and provides the following functionality:
 
* Internal handling of path joining.
* List "keys" (filename without RData-extension) or subsets of keys by providing a regular expression.
* Create, load, save and remove R object using a key-value syntax.
* Efficient apply functions on all files or on subsets.
* Flexible in-memory caching mechanism to avoid reading files multiple times.


## Installation

Install from CRAN:
```splus
install.packages("fail")
```

Alternatively, you can also install the latest development version from GitHub using the `devtools` package:
```splus
library(devtools)
install_github("fail", username="mllg")
library(fail)
```

## Usage

### Example files

For illustration we use a directory with multiple (result) files in it. You can create one in your current working directory by using the following small snippet. The next examples rely on them, so if you want to try FAIL out, better do this now.

```splus
path = file.path(getwd(), "results")
dir.create(path)
for (i in 1:10) {
    for (j in 1:10) {
        x = rnorm(100) 
        save(x, file = file.path(path, sprintf("result_%s_%02i.RData", letters[i], j)))
    }
}
list.files(path)
```

### Initialization

A FAIL is constructed with the `fail` function. You can specify the path to work on (defaults to the current working directory), the file extension (default: "RData"), and two logical flags:  a in-memory cache (default `FALSE`) and overwrite protection (default `FALSE`). The latter two can be switched locally in related functions and act more as a "global" default to save you some typing.

```splus
# initialize a FAIL on the previously created directory
results = fail("results")
print(results)
```

### Listing files

The path and further options are saved internally so everything you need to do is calling the `ls(pattern=NULL)` subfunction of the created object. The return value is always a character vector containing the keys (filenames without extension).

```splus
results$ls()

# restrict to  subsets using a regular expression
results$ls("^result_a")
```

### Loading R objects

FAIL provides two retrieval subfunctions: `get(key, cache)` and `as.list(keys, cache)`. `get` is handy to retrieve a single object by its key while `as.list` aims at loading multiple files into a named list. The `cache` argument defaults to the value specified in the constructor. If `cache` is set to `TRUE`, the objects will be stored in a memory cache which avoids loading the related files multiple times.

```splus
# single object
results$get("_a_")

# multiple objects
keys = results$ls("_a_") 
results$as.list(keys)

# all objects
results$as.list() # or as.list(results)

# read all files quickly into a list as one-liner
results = as.list(fail("results"))
```

### Saving R objects

The subfunction `put(..., li=list(), overwrite, cache)` stores all objects provided in the previously specified directory. You can pass arguments in a `key=vale` way or just use predefined variables (the variable names will then be looked up. You can furthermore pass a named list to `li` (see example). Again, the global flags `overwrite` and `cache` can be overwritten locally.

```splus
# add two files "foo.RData" and "bar.RData"
foo = 1
results$put(foo, bar = 2)

# you can also provide a named list, each item will be saved in a separate file
results$put(li = list(foo = 1, bar = 2))
```

### Removing R objects (and related files)

Of course you can also remove files. The subfunction `rm(keys)` takes a character vector of keys. Removed objects will also be purged from the cache.

```splus
results$remove("foo")
results$remove(results.ls("ar"))
```

### Applying functions over R objects

The typical R way to work with literally everything (because loops are ... kinda slow) is to apply functions over vectors or lists. The subfunction `apply(FUN, ..., keys, cache, simplify, use.names)` acts in  principle like a `sapply` (but has more sane defaults, because the error prone simplify is per default off). You can provide some keys (default is all keys) and the provided function `FUN` is applied iteratively on the related objects stored on the file system (or in the cache, respectively). `use.names` is per default `TRUE` and the function returns a named (possibly simplified) list with keys as list names.

```splus
# memory optimized lapply-like function
results$apply(mean, keys=results$ls("_a_"), simplify=TRUE)

# the same, but the list of all files will be created first
sapply(as.list(results), mean)
```

### More utility functions

The next snippet teases some more (for most users not that important) utility functions. If you are missing some important ones, contact me.

```splus
# show file size informations
results$size(unit="Kb")

# enable caching (can be switched on globally)
library(microbenchmark)
results$put(a = rnorm(100000))
microbenchmark(results$get("a"), results$get("a", cache=TRUE))

# simple cache control
results$cached()
results$clear()
results$cached()

# working with multiple directories (pseudo code)
# apply my_function_wrapper on all input data and save results in another directory
# (these kind of transformations might get a memory optimized function in the future)
input = fail(tempfile())
results = fail(tempfile())
results$put(li = input$apply(my_function_wrapper))
```
