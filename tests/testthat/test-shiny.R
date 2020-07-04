test_that("pivot2Output works", {

  tag <- pivot2Output("OUTPUT_ID")

  expect_is(tag, "shiny.tag")
  expect_true(grepl(pattern = "OUTPUT_ID", x = as.character(tag)))
})


test_that("renderPivot2 supported", {
  session <- shiny::MockShinySession$new()
  shiny::isolate({
    rp <- renderPivot2({
      pivot_table(nobel_laureates, rows = "category")
    }, background = "#EBCB8B")
    res <- rp(session, "name")
    expect_is(res, "list")
    expect_is(res$html, "html")
  })
})



test_that("pivotOutput works", {

  tag <- pivotOutput("OUTPUT_ID")

  expect_is(tag, "shiny.tag")
  expect_true(grepl(pattern = "OUTPUT_ID", x = as.character(tag)))
  expect_true(grepl(pattern = "OUTPUT_ID_clipboard", x = as.character(tag)))
  expect_true(grepl(pattern = "OUTPUT_ID_export_pptx", x = as.character(tag)))
  expect_true(grepl(pattern = "OUTPUT_ID_export_docx", x = as.character(tag)))
  expect_true(grepl(pattern = "OUTPUT_ID_export_xlsx", x = as.character(tag)))
})



test_that("renderPivot supported", {
  session <- shiny::MockShinySession$new()
  shiny::isolate({
    rp <- renderPivot({
      pivot_table(nobel_laureates, rows = "category")
    }, background = "#EBCB8B")
    res <- rp(session, "name")
    expect_is(res, "list")
    expect_is(res$html, "html")
  })
})



test_that("html_dependency_clipboard works", {

  dep <- html_dependency_clipboard()

  expect_is(dep, "html_dependency")
  expect_true(file.exists(system.file(dep$src$file, dep$script, package = "flexpivot")))
})





