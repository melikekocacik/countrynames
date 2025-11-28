test_that("country_name detects single country", {
  result <- country_name("I visited France last year")
  expect_equal(result, "France")
})


test_that("country_name detects multiple country", {
  result <- country_name("I visited France and UK last year")
  expect_equal(result, c("France", "UK"))
})

test_that("country_name detects NULL when no country", {
  result <- country_name("I visited somewhere last year")
  expect_equal(result, NULL)
})


test_that("howmany detects unique countries only", {
  result <- howmany("I visited France and France last year. France is great in the summer.")
  expect_equal(result, 1)
})
