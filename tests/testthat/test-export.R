test_that("export_pptx works with pivot_table", {

  pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")
  path <- tempfile(fileext = ".pptx")
  export_pptx(pt, path)

  expect_true(file.exists(path))

  unlink(path)
})


test_that("export_docx works with pivot_table", {

  pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")
  path <- tempfile(fileext = ".docx")
  export_docx(pt, path)

  expect_true(file.exists(path))

  unlink(path)
})


test_that("export_xlsx works with pivot_table", {

  pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")
  path <- tempfile(fileext = ".xlsx")
  export_xlsx(pt, path)

  expect_true(file.exists(path))

  unlink(path)
})



test_that("export_pptx works with pivot_format", {

  pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")
  pt <- pivot_format(pt)
  path <- tempfile(fileext = ".pptx")
  export_pptx(pt, path)

  expect_true(file.exists(path))

  unlink(path)
})


test_that("export_docx works with pivot_format", {

  pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")
  pt <- pivot_format(pt)
  path <- tempfile(fileext = ".docx")
  export_docx(pt, path)

  expect_true(file.exists(path))

  unlink(path)
})


test_that("export_xlsx works with pivot_format", {

  pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")
  pt <- pivot_format(pt)
  path <- tempfile(fileext = ".xlsx")
  export_xlsx(pt, path)

  expect_true(file.exists(path))

  unlink(path)
})
