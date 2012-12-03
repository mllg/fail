# fal

File Abstraction Layer (FAL) for R mimicking a key-value store.

This package simplifies working with RData files managed in directories.
A FAL is constructed on a single directory and provides convenient functionallity:
 
* Internal handling of path joining.
* List "keys" (filename without RData-extension) or subsets of keys by providing a regular expression.
* Create, load, save and remove R object using a key-value syntax.
* Effiently apply functions on all files or on subsets.
* Flexible in-memory caching mechanism to avoid reading files multiple times.
* Choose and mix between the closure interface (`results$get("a")`, `results$as.list()`, `results$ls()`) and a list-like interface (`results[["a"]]`, `as.list(results)`, `names(results)`).
  The latter can be turned off.


## Usage

For illustration assume we have a directory with multiple (result) files in it. You can create one in your current working directory by using this snippet:

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

```splus
# install package from github using devtools
library(devtools)
install_github("fal", username="mllg")

# load package
library(fal)

results = fal("results")
print(results)
```

### Listing files

```splus
# use
results$ls()
# -or-
names(results)

# get subsets using a regular expression
results$ls("result_a")
results$ls("_01")
```

### Loading R objects

```splus
# single objects
results$get("result_a_01")
results[["result_a_01"]]

# multiple objects
keys = results$ls("a") 
results$as.list(keys)
results[keys]
as.list(results, keys)

# all objects
results$as.list()
results[]
as.list(results)
```

### Saving R objects

```splus
# files will be named "foo.RData" and "bar.RData"
results$put(foo = 1, bar = 101)
results$put(li = list(foo = 2, bar = 102))
results[["foo"]] = 3
results[c("foo", "bar")] = c(4, 102)
```

### Removing R objects (and corresponding files)

```splus
results$remove("foo")
results[["bar"]] = NULL
results$remove(results$ls("result_j"))
```

### Applying functions over R objects

```splus
# memory-inefficent (list with all items will first be build)
sapply(results$as.list(), mean)
sapply(as.list(results), mean)

# specialized iterative version
results$apply(mean)
results$apply(mean, keys=results$ls("_a_"), simplify=TRUE)
```

### Other utility functions

```splus
# useful to considering if loading all files at once is possible
results$size(unit="Kb")

# use caching mechanism (can be enabled globally)
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
input = fal(tempfile())
results = fal(tempfile())
results$put(li = input$apply(my_function_wrapper))
```
