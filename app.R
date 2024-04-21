

# Load necessary libraries
library(shiny)
library(DT)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Shiny App with Editable Table, Dot Plot, and Michaelis-Menten Kinetics"),
  
  # Create a tabset panel
  tabsetPanel(
    # First tab
    tabPanel("Tab 1",
             sidebarLayout(
               sidebarPanel(
                 DTOutput("table1"),
                 numericInput("Km1", "Enter Km:", value = 1, min = 0),
                 numericInput("Vmax1", "Enter Vmax:", value = 1, min = 0),
                 textOutput("velocity1")
               ),
               
               mainPanel(
                 plotOutput("dotplot1")
               )
             )
    ),
    
    # Second tab
    tabPanel("Tab 2",
             sidebarLayout(
               sidebarPanel(
                 DTOutput("table2"),
                 numericInput("Km2", "Enter Km:", value = 1, min = 0),
                 numericInput("Vmax2", "Enter Vmax:", value = 1, min = 0),
                 textOutput("velocity2")
               ),
               
               mainPanel(
                 plotOutput("dotplot2")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Create two reactive dataframes with 2 columns and 10 rows
  df1 <- reactiveValues(data = data.frame(V1 = rep(NA, 10), V2 = seq(30, by = 30, length.out = 10)))
  df2 <- reactiveValues(data = data.frame(V1 = rep(NA, 10), V2 = seq(30, by = 30, length.out = 10)))
  
  # Render the editable tables
  output$table1 <- renderDT({df1$data}, editable = TRUE, options = list(dom = 't'))
  output$table2 <- renderDT({df2$data}, editable = TRUE, options = list(dom = 't'))
  
  # Update the dataframes when the tables are edited
  observeEvent(input$table1_cell_edit, {
    info <- input$table1_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    df1$data[i, j] <<- DT::coerceValue(v, df1$data[i, j])
  })
  observeEvent(input$table2_cell_edit, {
    info <- input$table2_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    df2$data[i, j] <<- DT::coerceValue(v, df2$data[i, j])
  })
  
  # Render the dot plots
  output$dotplot1 <- renderPlot({
    req(nrow(df1$data) > 1) # Ensure there's more than one row of data
    ggplot(df1$data, aes(x = df1$data[,1], y = df1$data[,2])) + geom_point()
  })
  output$dotplot2 <- renderPlot({
    req(nrow(df2$data) > 1) # Ensure there's more than one row of data
    ggplot(df2$data, aes(x = df2$data[,1], y = df2$data[,2])) + geom_point()
  })
  
  # Calculate and display the reaction velocities
  output$velocity1 <- renderText({
    req(input$Km1 > 0, input$Vmax1 > 0) # Ensure Km and Vmax are greater than 0
    S <- mean(df1$data[,1], na.rm = TRUE) # Use the mean of the first column as the substrate concentration
    v <- input$Vmax1 * S / (input$Km1 + S) # Calculate the reaction velocity
    paste("Reaction velocity (v) =", round(v, 2))
  })
  output$velocity2 <- renderText({
    req(input$Km2 > 0, input$Vmax2 > 0) # Ensure Km and Vmax are greater than 0
    S <- mean(df2$data[,1], na.rm = TRUE) # Use the mean of the first column as the substrate concentration
    v <- input$Vmax2 * S / (input$Km2 + S) # Calculate the reaction velocity
    paste("Reaction velocity (v) =", round(v, 2))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)