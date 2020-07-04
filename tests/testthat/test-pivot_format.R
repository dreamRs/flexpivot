test_that("pivot_format works: one row, one col", {

  pf <- pivot_format(pivot_table(nobel_laureates, "category", "gender"))

  expect_is(pf, "flextable")
  expect_is(pf, "flexpivot")
  expect_is(attr(pf, "data"), "pivot_table")
})

test_that("pivot_format works: one row", {

  pf <- pivot_format(pivot_table(nobel_laureates, "category"))

  expect_is(pf, "flextable")
  expect_is(pf, "flexpivot")
  expect_is(attr(pf, "data"), "pivot_table")
})

