library(shiny)
library(ggplot2)
library(rhandsontable)



# Define UI for application
ui <- fluidPage(
  titlePanel("BIOC192 Lab 2"),
  tabsetPanel(
    tabPanel("Exercise 1",
             rHandsontableOutput("table1"),
             plotOutput("absorbance_conc"),
             helpText("This graph represents the relationship between Concentration (µmol/L) and Absorbance. ",
                      "Each point on the graph corresponds to a sample. ",
                      "The red dashed line is the line of best fit. It goes through the origin.",
                      "The x-axis represents the Concentration (µmol/L) and the y-axis represents the Absorbance. ",
                      )
    ),
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
                      rHandsontableOutput("tableVmax"),
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
    
    #make the plot1
    ggplot(data1$df, aes(x = Concentration, y = Absorbance)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red", linetype = "dashed") + # Add line of best fit
      labs(x = "Concentration (µmol/L)", y = "Absorbance", title = "Absorbance vs Concentration (µmol/L)") +
      theme_minimal() +
      theme(axis.line.x = element_line(color = "black", size = 1),
            axis.line.y = element_line(color = "black", size = 1),
            plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(expand = c(0, 0), limits = c(0, max(data1$df$Concentration, na.rm = TRUE) * 1.1)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(data1$df$Absorbance, na.rm = TRUE) * 1.1)) 
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
      geom_smooth(data = subset_data, aes(y = Assay2), color = "red", method = "lm", se = FALSE, fullrange = TRUE, linetype = "dotted") +
      labs(x = "Time (seconds)", y = "Absorbance", title = "Alcohol Dehydrogenase Assay") +
      theme_minimal() +
      scale_y_continuous(expand = c(0,0), limits = c(0, max(data2$df$Assay1, data2$df$Assay2, na.rm = TRUE) * 1.1)) +
      scale_x_continuous(expand = c(0,0), breaks = seq(0, max(data2$df$Time, na.rm = TRUE), by = 60), 
                         minor_breaks = seq(0, max(data2$df$Time, na.rm = TRUE), by = 20)) +
      theme(legend.position = "bottomright") +
      theme(axis.line.x = element_line(color = "black", size = 1),
            axis.line.y = element_line(color = "black", size = 1),
            plot.title = element_text(hjust = 0.5)) +
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
    Conc4 = seq(from = 0, to = 0.3, length.out = 9)^1.25,
    Conc5 = seq(from = 0, to = 0.3, length.out = 9)^1.24,
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
    
    # Add an empty row, well it will be empty later but first we make the math work. 
    new_df <- rbind(new_df, c(5,10,20,40,80))
    
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
  data4 <- reactiveValues()
  
  # Update the data in the reactiveValues object when table4 changes
  observeEvent(input$table4, {
    data4$df <- hot_to_r(input$table4)
  })
 
  
  # Render the VvsS plot
  output$VvsS <- renderPlot({
    # Access the data from the reactiveValues object
    df <- data4$df
    
    # Convert df to a data frame
    df <- as.data.frame(df)
    
    # Fit the Michaelis-Menten equation to the data
    fit <- nls(deltaA ~ Vmax * Concentration / (Km + Concentration), 
               start = list(Vmax = max(df$deltaA, na.rm = TRUE), Km = median(df$Concentration, na.rm = TRUE)), 
               data = df)
    
    # Get the Vmax and Km from the fit
    Vmax <- coef(fit)["Vmax"]
    Km <- coef(fit)["Km"]
    
    # Create a ggplot2 dot plot
    ggplot(df, aes(x = Concentration, y = deltaA)) +
      geom_point() +
      geom_segment(aes(x = 0, xend = Km, y = Vmax/2, yend = Vmax/2, linetype = "Half Vmax"), color = "red") +  # Add horizontal line at half Vmax
      geom_segment(aes(x = Km, xend = Km, y = 0, yend = Vmax/2, linetype = "Km"), color = "blue") +   # Add vertical line at Km
      geom_segment(aes(x = 0, xend = max(df$Concentration)*1.1, y = Vmax, yend = Vmax, linetype = "Vmax"), color = "purple") +  # Add horizontal line at Vmax
      labs(x = "Substrate Concentration", y = "ΔA/min",title = "V vs [S]", linetype = "Parameters") +
      theme_minimal() +
      theme(axis.line.x = element_line(color = "black", size = 1),
           axis.line.y = element_line(color = "black", size = 1),
           plot.title = element_text(hjust = 0.5)) +
      scale_linetype_manual(values = c("dashed", "dashed", "dashed"))+
      scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$Concentration)*1.1))+
      scale_y_continuous(expand = c(0,0), limits = c(0, max(Vmax ,na.rm = TRUE) * 1.1))+
      coord_cartesian(xlim = c(0, NA), ylim = c(0, NA))  # Set the limits of the x and y axes to start at 0
  })
  
  
  
  
  
  
  # Create a reactive expression for table5
  table5 <- reactive({
    # Access the data from the reactiveValues object
    df <- data4$df
    
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
    df <- as.data.frame(table5())  # Convert the matrix to a dataframe
    
    # Fit a linear model
    fit <- lm(deltaA ~ Concentration, data = df)
    
    # Calculate the x-intercept (where y = 0)
    x_intercept <- -fit$coefficients[1] / fit$coefficients[2]
    
    # Create a ggplot2 dot plot
    ggplot(df, aes(x = Concentration, y = deltaA)) +
      geom_point() +
      geom_abline(intercept = fit$coefficients[1], slope = fit$coefficients[2], color = "red") +  # Draw the regression line manually
      geom_vline(xintercept = 0, color = "black") +  # Add a vertical line at 0 on the x-axis
      geom_hline(yintercept = 0, color = "black") +  # Add a horizontal line at 0 on the y-axis
      expand_limits(x = min(0, x_intercept)) +  # Extend the x-axis to include the x-intercept
      ylim(0, NA) +  # Set the starting point of the y-axis to 0
      labs(x = "1/Concentration", y = "1/Delta A") +
      theme_minimal()
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
