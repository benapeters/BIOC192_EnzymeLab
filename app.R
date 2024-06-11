library(shiny)
library(DT)
library(ggplot2)
library(rhandsontable)


# Define UI for application
ui <- fluidPage(
  titlePanel("Shiny App"),
  tabsetPanel(
    tabPanel("Exercise 1",
             DT::dataTableOutput("table1"),
             tags$br(),
             plotOutput("absorbance_conc")),
    tabPanel("Tab handsontable",
             rHandsontableOutput("table1h")
             ),
    tabPanel("Tab 2",
             DT::dataTableOutput("table2"),
             tags$br(),
             plotOutput("progressCurve")),
    tabPanel("Tab 3",
             fluidRow(
               column(6,
                      DTOutput("table3")
               ),
                tags$br(),
               column(6,
                      DTOutput("table4"),
                tags$br(),
                      plotOutput("VvsS"))
             )),
    tabPanel("Tab 4",
             fluidRow(
               column(6,
                      DTOutput("table5"),
                      plotOutput("lineweaver"))
             ))
  )
)



# Define server logic
server <- function(input, output) {
  
  #test hands on table 
  data1h <- reactiveValues(df = data.frame(Sample = c("Water", "1 in 8 dilution", "1 in 4 dilution", "1 in 2 dilution", "Undiluted"),
                                          Concentration = rep(0, 5),
                                          Absorbance = rep(0, 5),
                                          stringsAsFactors = FALSE))
  
  output$table1h <- renderRHandsontable({
    rhandsontable(data1h$df) %>%
      hot_col("Concentration", type = "numeric", strict = TRUE, allowInvalid = FALSE) %>%
      hot_col("Absorbance", type = "numeric", strict = TRUE, allowInvalid = FALSE)
  })
  #Define the initial data frame for Tab 1
   data1 <- reactiveValues(df = data.frame(Sample = c("Water", "1 in 8 dilution", "1 in 4 dilution", "1 in 2 dilution", "Undiluted"),
                                           Concentration = rep("", 5),
                                           Absorbance = rep("", 5),
                                           stringsAsFactors = FALSE))
  
   #Render the data table for Tab 1
   output$table1 <- DT::renderDataTable({
     DT::datatable(data1$df, editable = TRUE, options = list(dom = 't', paging = FALSE, searching = FALSE))
   })
  
  
  
  
  # Update the data frame for Tab 1 when the table is edited
   proxy1 = dataTableProxy('table1')
   observeEvent(input$table1_cell_edit, {
   info = input$table1_cell_edit
   str(info)
   i = info$row
   j = info$col
   v = info$value
   data1$df[i, j] <<- DT::coerceValue(v, data1$df[i, j])
   replaceData(proxy1, data1$df, resetPaging = FALSE)
   })
  
  
  
  
  # Render the absorbance vs concentration plot for Tab 1
  output$absorbance_conc <- renderPlot({
    req(nrow(data1$df) > 0)
    plot(data1$df$Concentration, data1$df$Absorbance, xlab = "Concentration", ylab = "Absorbance", main = "Absorbance vs Concentration")
  })
  
  # Define the initial data frame for Tab 2
  data2 <- reactiveValues(df = data.frame(Time = seq(20, 180, by = 20),
                                          Assay1 = rep(0, 9),
                                          Assay2 = rep(0, 9),
                                          stringsAsFactors = FALSE))
  
  # Convert Assay1 and Assay2 to numeric (if possible)
  observe({
    data2$df$Assay1 <- as.numeric(data2$df$Assay1)
    data2$df$Assay2 <- as.numeric(data2$df$Assay2)
  })
  
  # Render the data table for Tab 2
  output$table2 <- DT::renderDataTable({
    DT::datatable(data2$df, editable = TRUE, options = list(dom = 't', paging = FALSE, searching = FALSE))
  })

    
    # Update the data frame for Tab 2 when the table is edited
    proxy2 = dataTableProxy('table2')
    observeEvent(input$table2_cell_edit, {
      info = input$table2_cell_edit
      i = info$row
      j = info$col
      v = info$value
      data2$df[i, j] <<- DT::coerceValue(v, data2$df[i, j])
      replaceData(proxy2, data2$df, resetPaging = FALSE)
    })
    
    # Render the progress Curve scatter plot for Tab 2
    output$progressCurve <- renderPlot({
      req(nrow(data2$df) > 0)
      
      # Set y-axis limits
      ylim <- c(0, max(data2$df$Assay1, data2$df$Assay2, na.rm = TRUE) * 1.1)
      #design the plot
      plot(data2$df$Time, data2$df$Assay1, xlab = "Time", ylab = "Assay1", main = "Alcohol Dehydrogenase Assay", type = "p", col = "black", pch = 16, ylim = ylim)
      points(data2$df$Time, data2$df$Assay2, col = "red", pch = 17)
      legend("bottomright", legend = c("Assay1", "Assay2"), col = c("black", "red"), pch = c(16, 17))
    })
   
    #dataframe 3 generation. Currently it has values in for debugging.
    #later we need to swap it back from seq to rep with NA's so the 
    #students can enter their own data.
  data3 <- data.frame(
    Time = seq(20, 180, by = 20),
    Conc1 = seq(from = 0, to = 0.3, length.out = 9),
    Conc2 = seq(from = 0, to = 0.3, length.out = 9)^1.1,
    Conc3 = seq(from = 0, to = 0.3, length.out = 9)^1.2,
    Conc4 = seq(from = 0, to = 0.3, length.out = 9)^1.3,
    Conc5 = seq(from = 0, to = 0.3, length.out = 9)^1.4
  )
  output$table3 <- renderDT({
    datatable(data3, editable = TRUE, options = list(dom = 't', paging = FALSE, searching = FALSE))
  })
  data4 <- data.frame(
    Conc1 = c((data3$Conc1[2] - data3$Conc1[1])*3, 1),
    Conc2 = c((data3$Conc2[2] - data3$Conc2[1])*3, 2),
    Conc3 = c((data3$Conc3[2] - data3$Conc3[1])*3, 3),
    Conc4 = c((data3$Conc4[2] - data3$Conc4[1])*3, 4),
    Conc5 = c((data3$Conc5[2] - data3$Conc5[1])*3, 5)
  )
  

  
  #round data4 table down to 2 dp
  data4 <- as.data.frame(sapply(data4, round, 2))
  
  
  output$table4 <- renderDT({
    row.names(data4) <- c("∆A per min", "Concentration")
    datatable(data4, editable = 't', options = list(dom = 't', paging = FALSE, searching = FALSE))
  })
 
#transpose data4 for plotting
  data4_transposed <- as.data.frame(t(data4))
  data4_transposed <- data4_transposed[-1,]
  col1 <- names(data4_transposed)[1]
  col2 <- names(data4_transposed)[2]
  # Create the scatter plot
  output$VvsS <- renderPlot({
    ggplot(data4_transposed, aes_string(x = col2, y = col1)) +
      geom_point(size = 3, position = position_dodge(width = 0.5)) +
      labs(x = "Calculation", y = "Value") +
      theme_minimal()
  })
    
  data5 <- data.frame(
    
    Conc1 = c(1/data4$Conc1[1],1/data4$Conc1[2]),
    Conc2 = c(1/data4$Conc2[1],1/data4$Conc2[2]),
    Conc3 = c(1/data4$Conc3[1],1/data4$Conc3[2]),
    Conc4 = c(1/data4$Conc4[1],1/data4$Conc4[2]),
    Conc5 = c(1/data4$Conc5[1],1/data4$Conc5[2])
    
  )
  
  
  #round the table down to 2 dp. 
  data5 <- as.data.frame(sapply(data5, round, 2))
  
  output$table5 <- renderDT({
    row.names(data5) <- c("1/[s]", "1/[v]")
    datatable(data5, editable = 'f', options = list(dom = 't', paging = FALSE, searching = FALSE))
  })
  #create the lineweaver burk plot
  data5_transposed <- as.data.frame(t(data5))
  data5_transposed <- data5_transposed[-1,]
  col1 <- names(data5_transposed)[1]
  col2 <- names(data5_transposed)[2]
  # Create the scatter plot
  output$lineweaver <- renderPlot({
    ggplot(data5_transposed, aes_string(x = col2, y = col1)) +
      geom_point(size = 3, position = position_dodge(width = 0.5)) +
      labs(x = "Calculation", y = "Value") +
      theme_minimal()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
