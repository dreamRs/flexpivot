test_that("pivot_addin works", {

  ui <- pivot_addin_ui(LETTERS)
  server <- pivot_addin_server()

  expect_is(ui, "shiny.tag.list")
  expect_is(server, "function")
})
