context("Constructor")

test_that("Creation of directories and checking of existing files", {
  path = tempfile()
  fal(path) # works on new dirs
  fal(path) # works on existing dirs
  path = tempfile()
  file.create(path)
  expect_error(fal(path))
})

test_that("Constructor checks input", {
  path = tempfile()
  expect_error(fal(path, extension="^"))
  expect_error(fal(path, extension=".RData"))
})
