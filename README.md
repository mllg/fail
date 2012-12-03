# fail

File Abstraction Interface Layer (FAIL) for R mimicking a key-value store.

This package simplifies working with RData files managed in directories.
A FAIL is constructed on a single directory and provides convenient functionality:
 
* Internal handling of path joining.
* List "keys" (filename without RData-extension) or subsets of keys by providing a regular expression.
* Create, load, save and remove R object using a key-value syntax.
* Efferently apply functions on all files or on subsets.
* Flexible in-memory caching mechanism to avoid reading files multiple times.


## Installation

Install from GitHub using the `devtools` package:
```splus
library(devtools)
install_github("fail", username="mllg")
library(fail)
```

## Usage

### Example files

For illustration assume we have a directory with multiple (result) files in it. You can create one in your current working directory by using this small snippet:

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
# initialize a FAIL on the previously created directory
results = fail("results")
print(results)
```

### Listing files

```splus
# get all keys for previously defined (and internally stored) directory
results$ls()

# restrict to  subsets using a regular expression
results$ls("^result_a")
```

### Loading R objects

```splus
# single object
results$get("_a_")

# multiple objects
keys = results$ls("_a_") 
results$as.list(keys)

# all objects
results$as.list() # or as.list(results)
```

### Saving R objects

```splus
# add two files "foo.RData" and "bar.RData"
results$put(foo = 1, bar = 2)

# you can also provide a named list, each item will be saved in a separate file
results$put(li = list(foo = 1, bar = 2))
```

### Removing R objects (and related files)

```splus
results$remove("foo")
results$remove(results.ls("ar"))
```

### Applying functions over R objects

```splus
# memory optimized lapply-like function
results$apply(mean, keys=results$ls("_a_"), simplify=TRUE)

# the same, but the list of all files will be created first
sapply(as.list(results), mean)
```

### More utility functions

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
