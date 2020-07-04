# From ?data.table::cube
n <- 100L
set.seed(42)
DF <- data.frame(
  color = sample(c("green","yellow","red"), n, TRUE),
  colorF = factor(sample(c("green","yellow","red"), n, TRUE), levels = c("green","yellow","red"), ordered = TRUE),
  year = as.Date(sample(paste0(2011:2015,"-01-01"), n, TRUE)),
  statusF = as.factor(sample(c("removed","active","inactive","archived"), n, TRUE)),
  status = sample(c("removed","active","inactive","archived"), n, TRUE),
  amount = sample(1:5, n, TRUE),
  value = sample(c(3, 3.5, 2.5, 2), n, TRUE),
  stringsAsFactors = FALSE
)

test_that("pivot_table works with one row", {

  PT <- pivot_table(data = DF, rows = "color")

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), 3L)
  expect_equal(nrow(PT), length(unique(DF$color)) + 1)
  expect_true("color" %in% names(DF))
  expect_identical(tail(PT$color, 1), "Total")
})


test_that("pivot_table works with data.table", {

  DT <- as.data.table(DF)

  PT <- pivot_table(data = DT, rows = "color")

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), 3L)
  expect_equal(nrow(PT), length(unique(DT$color)) + 1)
  expect_true("color" %in% names(DT))
  expect_identical(tail(PT$color, 1), "Total")
})


test_that("pivot_table works without total", {

  PT <- pivot_table(data = DF, rows = "color", total = FALSE)

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), 3L)
  expect_equal(nrow(PT), length(unique(DF$color)))
  expect_true("color" %in% names(DF))
  expect_false("Total" %in% PT$color)
})


test_that("pivot_table works with two rows", {

  PT <- pivot_table(data = DF, rows = c("color", "status"))

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), 4L)
  expect_equal(nrow(PT), nrow(unique(DF[, c("color", "status")])) + length(unique(DF$color)) + length(unique(DF$status)) + 1)
  expect_true("color" %in% names(DF))
  expect_true("status" %in% names(DF))
  expect_identical(tail(PT$color, 1), "Total")
  expect_identical(tail(PT$status, 1), "Total")
})

test_that("pivot_table works with one row and one col", {

  PT <- pivot_table(data = DF, rows = "color", cols = "status")

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), 2 + length(unique(DF$status)) + 1)
  expect_equal(nrow(PT), (length(unique(DF$color)) + 1) * 4)
  expect_true("color" %in% names(DF))
  expect_identical(tail(PT$color, 1), "Total")
  expect_identical(tail(names(PT), 1), "Total")
})

test_that("pivot_table works with one row and two cols", {

  PT <- pivot_table(data = DF, rows = "color", cols = c("status", "amount"))

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), (length(unique(DF$status)) + 1) * (length(unique(DF$amount)) + 1) + 2)
  expect_equal(nrow(PT), (length(unique(DF$color)) + 1) * 4)
  expect_true("color" %in% names(DF))
  expect_identical(tail(PT$color, 1), "Total")
  expect_identical(tail(names(PT), 1), "Total_|_Total")
})


test_that("pivot_table works with weights", {

  PT <- pivot_table(data = DF, rows = "colorF", wt = "value")

  expect_is(PT, "pivot_table")
  expect_equal(ncol(PT), 3L)
  expect_equal(nrow(PT), length(unique(DF$color)) + 1)
  expect_true("color" %in% names(DF))
  expect_identical(tail(as.character(PT$colorF), 1), "Total")
  expect_equal(sum(PT$n), sum(DF$value) * 2)

  expect_error(pivot_table(data = DF, rows = "colorF", wt = "DONTEXIST"))
})


