# fail

File Abstraction Interface Layer (FAIL) for R mimicking a key-value store.

This package simplifies working with RData files managed in directories.
A FAIL is constructed on a single directory and provides convenient functionality:
 
* Internal handling of path joining.
* List "keys" (filename without RData-extension) or subsets of keys by providing a regular expression.
* Create, load, save and remove R object using a key-value syntax.
* Efferently apply functions on all files or on subsets.
* Flexible in-memory caching mechanism to avoid reading files multiple times.
* Choose and mix between the closure interface (`results$get("a")`, `results$as.list()`, `results$ls()`) and a list-like interface (`results[["a"]]`, `as.list(results)`, `names(results)`).
  The latter can be turned off.

## Installation

Install from GitHub using the `devtools` package:
```splus
library(devtools)
install_github("fail", username="mllg")
library(fail)
```

## Usage

### Example files

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
# initialize a FAIL on the previously created directory
results = fail("results")
print(results)
```

### Listing files

```splus
### list files
# closure interface
results$ls()
# list-like interface
names(results)

### get subsets using a regular expression
# closure interface
results$ls("result_a")
# list-like interface 
subset(names(results), grepl("result_a", names(results)))
```

### Loading R objects

```splus
### single objects
# closure interface
results$get("result_a_01")
# list-like interface 
results[["result_a_01"]]

### multiple objects
keys = results$ls("a") 
# closure interface
results$as.list(keys)
# list-like interface 
results[keys]

### all objects
# closure interface
results$as.list()
# list-like interface 
as.list(results) 
results[] 
```

### Saving R objects

```splus
### new files will be named "foo.RData" and "bar.RData"
# closure interface
results$put(foo = 1, bar = 2)
results$put(li = list(foo = 1, bar = 2))
# list-like interface
results[["foo"]] = 1
results[c("foo", "bar")] = 1:2
```

### Removing R objects (and corresponding files)

```splus
# closure interface
results$remove("foo")
results$remove(results$ls("result_j"))
# list-like interface
results[["bar"]] = NULL
keys = names(results)
results[keys[grepl("result_j", keys)]] = NULL
```

### Applying functions over R objects

```splus
# closure interface, memory optimized
results$apply(mean, keys=results$ls("_a_"), simplify=TRUE)

# list-like interface
keys = names(results)
sapply(as.list(results), mean, keys[grepl("_a_", keys)], simplify=TRUE)
```

### More utility functions

```splus
# show file size informations
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
input = fail(tempfile())
results = fail(tempfile())
results$put(li = input$apply(my_function_wrapper))
```
