library(shiny)
library(DT)

ui <- fluidPage(
  DTOutput("table")
)

server <- function(input, output, session) {
  output$table <- renderDT({
    datatable(iris, options = list(pageLength = 5))
  })
}

shinyApp(ui, server)