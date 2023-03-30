library(shiny)
library(data.table)
library(DT)
library(ggplot2)
library(tidyr)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Uploading Files"),
  fileInput("upload", NULL, label = "Please upload CSV File", buttonLabel = "Upload...", accept = ".csv", multiple = FALSE),
  tableOutput("files"),
  
  numericInput("Tail", "Tail Factor:", value = 1.1, min = 0, max = 100),
  verbatimTextOutput("value"),
  dataTableOutput("myTable"),
  plotOutput("myPlot")
)

server <- function(input, output, session) {
  
  read_data <- reactive ({
    req(input$upload)
    read.csv(input$upload$datapath)
  })
  
  # show the uploaded file in a table
  output$files <- renderTable({
    read_data()
  }) 
  
  # show the value of Tail factor
  output$value <- renderText({ input$Tail })
  
  # calculate n
  n <- reactive({
    req(read_data())
    length(unique(read_data()$Loss_Year))
  })
  
  # create the table
  my_data <- reactive({
    data <- data.table(read_data())
    setorderv(data, c("Loss_Year", "Development_Year"))
    data[, "Cumulative Paid Claims" := cumsum(`Amount_of_Claims_Paid`), by = `Loss_Year`]
    
    data <- dcast(
      data[, -"Amount_of_Claims_Paid"], `Loss_Year` ~ `Development_Year`,
      value.var = "Cumulative Paid Claims"
    )
    
    n_row <- NROW(data)
    
    for (j in 3L:NCOL(data)) {
      row_i <- n_row - j + 3L
      set(
        data, i = n_row:row_i, j = j,
        value = sum(data[1L:(row_i - 1L), ..j]) /
          sum(data[1L:(row_i - 1L), j - 1L, with = FALSE]) *
          data[n_row:row_i, j - 1L, with = FALSE]
      )
    }
    data[, "4" := (input$Tail * data[,4])]
    data
  })
  
  output$myTable <- renderDataTable({
    datatable(as.data.frame(my_data()), editable = TRUE)
    setDT(my_data())
  })
  
  output$myPlot <- renderPlot({
    data_melted <- melt(my_data(), id.vars = c("Loss_Year"), measure.vars = c("1","2","3","4"), variable.name = "Development_Year", value.name = "value")
    ggplot(data_melted, aes(x = Development_Year, y = value, color = factor(Loss_Year))) +
      geom_line(size = 1, aes(group = Loss_Year)) +
      geom_point() +
      geom_text(aes(label = round(value, digits = 1)), hjust = 0.6, vjust = -1, size = 3.5) +
      scale_color_discrete(name = "Loss Year") +
      labs(title = "Cumulative Paid Claims and Projections by Development Year",
           x = "Development Year",
           y = "Cumulative Paid Claims") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)
