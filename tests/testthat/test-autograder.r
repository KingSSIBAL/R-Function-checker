test_that("autograder function exists", {
  expect_true(exists("autograder", mode = "function"))
})

test_that("cpp_fetch_from_github is callable", {
  expect_true(exists(".cpp_fetch_from_github", mode = "function"))
})

test_that("cpp_compare_identical is callable", {
  expect_true(exists(".cpp_compare_identical", mode = "function"))
})

test_that("cpp_get_testdata_url is callable", {
  expect_true(exists(".cpp_get_testdata_url", mode = "function"))
})

test_that("autograder requires function_name argument", {
  expect_error(autograder(), "missing")
})

test_that("autograder validates function_name is character", {
  expect_error(autograder(123), "character")
})

test_that("autograder validates function_name is single string", {
  expect_error(autograder(c("a", "b")), "single")
})
