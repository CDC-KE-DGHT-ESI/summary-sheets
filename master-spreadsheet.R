library(openxlsx2)

# create a workbook from scratch
wb <- wb_workbook(title = "A demo workbook")

wb <- wb_add_worksheet(wb, sheet = "my data")

# start writing a banded data table from the B2 cell.
# wb$add_data_table(x = mtcars, 
#                   dims = "B2", 
#                   banded_rows = TRUE, 
#                   #table_style = "TableStyleLight16",
#                   
#                   )

# use Excel's dimensions for mtcars
# As we want to write mtcars starting in cell B2
# It helps keeping the cells relative to x. (the data)
f_dims <- function(..., select = NULL) {
  wb_dims(x = mtcars, 
          from_row = 2, 
          from_col = "B", 
          ..., 
          select = select)
}

# Modify column names appearance
wb$add_fill(dims = f_dims(select = "col_names"), 
            color = wb_color("black"))

wb$add_font(dims = f_dims(select = "col_names"), 
            color = wb_color("white"), 
            size = 13, 
            bold = TRUE)

# modify data appearance (not column names)
wb$add_font(dims = f_dims(select = "data"), italic = TRUE)
