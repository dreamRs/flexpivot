library(flexpivot)
data("nobel_laureates")

pt <- pivot_table(nobel_laureates, rows = "category", cols = "gender")

# To PowerPoint
path_pptx <- tempfile(fileext = ".pptx")
export_pptx(pt, path_pptx)
browseURL(path_pptx)


# To Word
path_docx <- tempfile(fileext = ".docx")
export_docx(pt, path_docx)
browseURL(path_docx)


# To Excel
path_xlsx <- tempfile(fileext = ".xlsx")
export_xlsx(pt, path_xlsx)
browseURL(path_xlsx)


# Clean up
unlink(path_pptx)
unlink(path_docx)
unlink(path_xlsx)
