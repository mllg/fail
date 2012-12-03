context("S3")

test_that("single", {
  path = tempfile()
  f = fail(path)

  f["a"] = 1
  expect_equal(f$ls(), "a")
  f[c("a", "b")] = 1:2
  expect_equal(names(f), letters[1:2])
  expect_equal(f[c("a", "b")], setNames(as.list(1:2), letters[1:2]))
})

test_that("double", {
  path = tempfile()
  f = fail(path)

  f[["a"]] = 1
  expect_equal(names(f), "a")
  expect_equal(f[["a"]], 1)
})
