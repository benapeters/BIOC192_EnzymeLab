library(shiny)
library(ggplot2)
library(rhandsontable)


# Define UI for application
ui <- fluidPage(
  titlePanel("BIOC192 Lab 2"),
  tabsetPanel(
    tabPanel("Exercise 1",
             rHandsontableOutput("table1"),
             plotOutput("absorbance_conc")),
    tabPanel("Exercise 2",
             rHandsontableOutput("table2"),
             tags$br(),
             plotOutput("progressCurve")),
    tabPanel("Exercise 3",
             fluidRow(
               column(6,
                      rHandsontableOutput("table3")
               ),
               tags$br(),
               column(6,
                      rHandsontableOutput("table4"),
                      
                      tags$br(),
                      plotOutput("VvsS"))
             )),
    tabPanel("Exercise 4",
             fluidRow(
               column(6,
                      rHandsontableOutput("table5"),
                      plotOutput("lineweaver"))
             ))
  )
)



# Define server logic
server <- function(input, output) {
  
  
  #Define the initial data frame for Exercise 1
  
  #note to self, make the first values 0,0 for the water sample. 
  data1 <- reactiveValues(df = data.frame(Sample = c("Water", "1 in 8 dilution", "1 in 4 dilution", "1 in 2 dilution", "Undiluted"),
                                          Concentration = rep(NA, 5),
                                          Absorbance = rep(NA, 5),
                                          stringsAsFactors = FALSE))
  
  # Convert Assay1 and Assay2 to numeric (if possible)
  observe({
    data1$df$Absorbance <- as.numeric(data1$df$Absorbance)
    data1$df$Concentration <- as.numeric(data1$df$Concentration)
  })
  
  #Render the data table for Tab 1
  output$table1 <- renderRHandsontable({
    rhandsontable(data1$df) %>%
      hot_col("Concentration", type = "numeric", strict = FALSE, allowInvalid = FALSE) %>%
      hot_col("Absorbance", type = "numeric", strict = FALSE, allowInvalid = FALSE)
  })
  
  observe({
    if (!is.null(input$table1)) {
      data1$df <- hot_to_r(input$table1)
    }
  })
  
  
  
  # Render the absorbance vs concentration plot for Tab 1
  output$absorbance_conc <- renderPlot({
    
    # Ensure the Concentration and Absorbance columns are not NULL
    req(data1$df$Concentration, data1$df$Absorbance)
    
    #make the plot1
    ggplot(data1$df, aes(x = Concentration, y = Absorbance)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", linetype = "dashed") + # Add line of best fit
      labs(x = "Concentration (µmol/L)", y = "Absorbance", title = "Absorbance vs Concentration (µmol/L)") +
      theme_minimal() +
      theme(axis.line.x = element_line(color = "black", size = 1),
            axis.line.y = element_line(color = "black", size = 1),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, max(data1$df$Concentration, na.rm = TRUE) * 1.1)) + # Add this line
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(data1$df$Absorbance, na.rm = TRUE) * 1.1)) # Add this line
  })
  
  # Define the initial data frame for Tab 2
  data2 <- reactiveValues(df = data.frame(
    Time = seq(20, 180, by = 20),
    Assay1 = rep(NA, 9),
    Assay2 = rep(NA, 9),
    stringsAsFactors = FALSE
  ))
  # Convert Assay1 and Assay2 to numeric (if possible)
  observe({
    data2$df$Assay1 <- as.numeric(data2$df$Assay1)
    data2$df$Assay2 <- as.numeric(data2$df$Assay2)
  })
  # Render the data table for Tab 2
  output$table2 <- renderRHandsontable({
    rhandsontable(data2$df) %>% 
      hot_col("Time", type = "numeric", strict = TRUE, allowInvalid = FALSE) %>%
      hot_col("Assay1", type = "numeric", strict = TRUE, allowInvalid = FALSE) %>%
      hot_col("Assay2", type = "numeric", strict = TRUE, allowInvalid = FALSE)
  })
  # Update dataframe after inputs
  observe({
    if (!is.null(input$table2)) {
      data2$df <- hot_to_r(input$table2)
    }
  })
  
  # Render the progress Curve scatter plot for Tab 2
  output$progressCurve <- renderPlot({
    req(nrow(data2$df) > 0)
    
    # Subset the data
    subset_data <- data2$df[data2$df$Time <= 40, ]
    
    
    # Create the plot2
    ggplot(data2$df, aes(x = Time)) +
      geom_point(aes(y = Assay1), color = "black",size = 3) +
      geom_smooth(data = subset_data, aes(y = Assay1), color = "black", method = "lm", se = FALSE, fullrange = TRUE, linetype = "dotted") +
      geom_point(aes(y = Assay2), color = "red", size = 3) +
      geom_smooth(data = subset_data, aes(y = Assay2), color = "red", method = "lm", se = FALSE, fullrange = TRUE) +
      labs(x = "Time (seconds)", y = "Absorbance", title = "Alcohol Dehydrogenase Assay") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, max(data2$df$Assay1, data2$df$Assay2, na.rm = TRUE) * 1.1)) +
      scale_x_continuous(breaks = seq(0, max(data2$df$Time, na.rm = TRUE), by = 60), 
                         minor_breaks = seq(0, max(data2$df$Time, na.rm = TRUE), by = 20)) +
      theme(legend.position = "bottomright") +
      scale_color_manual(values = c("black", "red"), labels = c("Assay1", "Assay2")) +
      theme(panel.grid.major = element_line(colour = "grey", linetype = "solid"),
            axis.line = element_line(colour = "black", size = 1, linetype = "solid"))
    
    
  })
  
  
  
  
  #dataframe 3 generation. Currently it has values in for debugging.
  #later we need to swap it back from seq to rep with NA's so the 
  #students can enter their own data.
  # Define the initial data frame
  data3 <- reactiveValues(df = data.frame(
    Time = seq(20, 180, by = 20),
    Conc1 = seq(from = 0, to = 0.3, length.out = 9)^1.5,
    Conc2 = seq(from = 0, to = 0.3, length.out = 9)^1.4,
    Conc3 = seq(from = 0, to = 0.3, length.out = 9)^1.3,
    Conc4 = seq(from = 0, to = 0.3, length.out = 9)^1.2,
    Conc5 = seq(from = 0, to = 0.3, length.out = 9)^1.1,
    stringsAsFactors = FALSE
  ))
  
  # Render the data table
  output$table3 <- renderRHandsontable({
    rhandsontable(data3$df)
  })
  
  # Update the data frame when the table is edited
  observe({
    if (!is.null(input$table3)) {
      data3$df <- hot_to_r(input$table3)
    }
  })
  
  
  # Create a reactive expression for table4
  table4 <- reactive({
    # Access the data from data3
    df <- data3$df
    
    # Calculate the difference between the second and first value in each column
    new_df <- data.frame(lapply(df, function(x) (x[2] - x[1])*3))
    
    # Remove the first column
    new_df <- new_df[,-1]
    
    # Add an empty row
    new_df <- rbind(new_df, rep(1, ncol(new_df)))
    
    # Transpose the data frame
    new_df <- t(new_df)
    
    # Add column names
    colnames(new_df) <- c("deltaA", "Concentration")
    
    return(new_df)
  })
  
  # Render table4
  output$table4 <- renderRHandsontable({
    rhandsontable(table4(), readOnly = FALSE) 
  })
  
  # Create a reactiveValues object to store the data
  data <- reactiveValues()
  
  # Update the data in the reactiveValues object when table4 changes
  observeEvent(input$table4, {
    data$df <- hot_to_r(input$table4)
  })
  
  # Render the VvsS plot
  output$VvsS <- renderPlot({
    # Access the data from the reactiveValues object
    df <- data$df
    
    # Create a ggplot2 dot plot
    ggplot(df, aes(x = Concentration, y = deltaA)) +
      geom_point() +
      labs(x = "Concentration", y = "Delta A") +
      theme_minimal()
  })
  # Create a reactive expression for table5
  table5 <- reactive({
    # Access the data from the reactiveValues object
    df <- data$df
    
    # Calculate the reciprocal of each value in the data frame
    new_df <- 1 / df
    
    return(new_df)
  })
  
  # Render table5
  output$table5 <- renderRHandsontable({
    rhandsontable(table5(), readOnly = TRUE) 
  })
  
  # Render the Lineweaver-Burk plot
  output$lineweaver <- renderPlot({
    # Access the data from table5
    df <- table5()
    
    # Create a ggplot2 dot plot
    ggplot(df, aes(x = Concentration, y = deltaA)) +
      geom_point() +
      labs(x = "1/Concentration", y = "1/Delta A") +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
