context("list, get, put")

test_that("list, get, put", {
  path = tempfile()
  f = fal(path)

  expect_equal(f$ls(), character(0L))
  expect_equal(f$put(a = 1, b = 2), letters[1:2])
  expect_equal(f$ls(), letters[1:2])
  expect_equal(f$put(li = list(c = 3)), letters[3])
  expect_equal(f$ls(), letters[1:3])
  f$put(d = 4, li = list(e = 5))
  expect_equal(f$ls(), letters[1:5])

  expect_equal(f$get("a"), 1)
  x = f$as.list()
  y = setNames(as.list(1:5), letters[1:5])
  expect_equal(x, y)

  # pattern works
  expect_equal(f$ls("^[ab]"), letters[1:2])
  expect_equal(f$ls("x"), character(0L))

  # invalid keys and empty sets
  expect_error(f$get())
  expect_equal(f$get("not_existing"), NULL)
  expect_error(f$put(1))
  expect_error(f$put(li=list("a-b" = 1)))
  expect_error(f$put(li=list("..a" = 1)))
  expect_error(f$put(li=list("..1" = 1)))
  expect_equal(f$put(), character(0L))

  # overwrite protect
  expect_error(f$put(a=1, overwrite=FALSE))

  # cache
  expect_equal(f$cached(), character(0L))
  f$get("a", cache=TRUE)
  expect_equal(f$cached(), "a")
  f$clear()
  expect_equal(f$cached(), character(0L))
})
