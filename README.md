# fail

File Abstraction Interface Layer (FAIL) for R, mimicking a key-value store.

This package simplifies working with RData files managed in directories.
A FAIL operates non-recursively on a single directory and provides the following functionality:

* Internal handling of path joining.
* List "keys" (filename without RData-extension) or subsets of keys by providing regular expressions.
* Create, load, save and remove R objects using a key-value syntax.
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

For illustration we create a directory with multiple (result) files in it.
You can create one in your current working directory by using the following small snippet. All next examples rely on them, so you better do this now.

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

A FAIL is constructed with the `fail` function: `fail(path = getwd(), extension = ".RData", use.cache = FALSE)`.
You can specify the path to work on (defaults to the current working directory), the file extension (default: "RData"), and a logical flag to enable or disable the caching mechanism per default (default: `FALSE`).

```splus
# initialize a FAIL on the previously created directory
library(fail)
results = fail("results")
print(results)
```

### Listing files

The path and further options are saved internally so everything you need to do is calling the `ls(pattern=NULL)` subfunction of the created object.
The return value is always a character vector containing the keys (filenames without extension).

```splus
results$ls()

# restrict to  subsets using a regular expression
results$ls("^result_a")
```

### Retrieving R objects

FAIL provides two retrieval subfunctions: `get(key, use.cache)` and `as.list(keys, use.cache)`.
`get` is handy to retrieve a single object by its key while `as.list` loads multiple files into a named list.
The `use.cache` argument defaults to the value specified in the constructor.
If `use.cache` is set to `TRUE`, the objects will be stored in memory so that multiple calls to `get` or `as.list` do not cause multiple disk reads.

Furthermore the subfunction `assign(keys, envir, use.cache)` assigns objects to a provided environment `envir` which defaults to the current.

```splus
# single object
results$get("result_a_01")

# multiple objects
keys = results$ls("_a_")
results$as.list(keys)

# all objects
results$as.list() # or as.list(results)

# read all files quickly into a list as one-liner
as.list(fail("results"))

# assign two variables into the current environment
results$assign(c("result_a_01", "result_a_02"))
mean(result_a_01)
```

### Saving R objects

The subfunction `put(..., keys, li=list(), use.cache)` stores all objects provided to the directory specified in the constructor.
You can pass arguments in a `key=vale` syntax or just use predefined variables (the variable names will then be looked up).
You can furthermore pass a named list to `li` (see example).
The argument `keys` can be used to overwrite the names for the objects passed via `...` which is useful in some scenarios, e.g. together with `do.call`.
Again, the global flag `use.cache` can be overwritten locally.

```splus
# add two files "foo.RData" and "bar.RData"
foo = 1
results$put(foo, bar = 2)

# provide a named list, each item will be saved in a separate file
results$put(li = list(foo = 1, bar = 2))
```

### Removing R objects (and related files)

Of course you can also remove files. The subfunction `rm(keys)` takes a character vector of keys. Removed objects will also be purged from the cache.

```splus
results$remove("foo")
results$remove(results$ls("ar")) # matches bar
```

### Applying functions over R objects

The subfunction `apply(FUN, ..., keys, cache, simplify = FALSE, use.names = TRUE)` acts in  principle like a `sapply` (but has more sane defaults, because the error prone simplify is per default off).
You can provide some keys (default is all keys) and the provided function `FUN` is applied on the objects stored on the file system (or in the cache).
`use.names` defaults to `TRUE`. The function returns a named (possibly simplified) list with keys as list names.
The advantage over manually applying a function with `sapply` is the iterative approach: the complete list containing all objects is not created to keep memory consumption low.

Sometimes the functionality of `lapply` does not suffice.
Therefore the package also ships with a version of `mapply` as `mapply(FUN, ..., keys, use.cache, moreArgs = NULL, simplify = FALSE, use.names = TRUE)`.
The provided function `FUN` must have the formals `key` and `value` to which the keys and corresponding objects are passed.

```splus
# memory optimized lapply-like function
results$apply(mean, simplify=TRUE)

# identical, but the list of all objects will be created first
sapply(as.list(results), mean)

# map function scale and store results (in a temporary directory)
scaled = fail(tempfile())
scaled$put(li = results$apply(scale)) # memory inefficient
results$mapply(function(key, value) scaled$put(scale(value), keys = key)) # memory efficent
scaled$ls()
```

### More utility functions

The next snippet teases some more (for most users not that important) utility functions.
If you are missing some important ones, please contact me.

```splus
# show file size informations
results$size(unit="kB")

# enable caching (can be switched on globally)
library(microbenchmark)
results$put(a = rnorm(100000))
microbenchmark(results$get("a"), results$get("a", use.cache=TRUE))

# simple cache control
results$cached()
results$clear()
results$cached()
```
