test_that("pivot_addin works", {

  ui <- pivot_ui("ID", LETTERS)
  # server <- pivot_server()

  expect_is(ui, "shiny.tag")
  # expect_is(server, "function")
})
