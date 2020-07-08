test_that("pivot_format works: one row, one col", {

  pt <- pivot_table(nobel_laureates, "category", "gender")
  pf <- pivot_format(pt)

  expect_is(pf, "flextable")
  expect_is(pf, "flexpivot")
  expect_is(attr(pf, "data"), "pivot_table")

  expect_error(pivot_format(nobel_laureates))
})

test_that("pivot_format works: one row", {

  pf <- pivot_format(pivot_table(nobel_laureates, "category"))

  expect_is(pf, "flextable")
  expect_is(pf, "flexpivot")
  expect_is(attr(pf, "data"), "pivot_table")
})


test_that("pivot_format works with labels", {

  pf <- pivot_format(
    pivot = pivot_table(nobel_laureates, "category"),
    labels = pivot_labels(
      row = "CATEGORY", n = "COUNT"
    )
  )

  expect_is(pf, "flextable")
  expect_is(pf, "flexpivot")
  expect_is(attr(pf, "data"), "pivot_table")
})

