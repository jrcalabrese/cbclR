#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinythemes)
library(rsconnect)
library(DT)
library(expss)
library(reshape2)

source("ui.R")

server <- function(input, output) {
  
  my_data <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote) %>%
      clean_my_data()
    
  })
  
  output$contents <- renderPlot({
    
    req(my_data())
    
    if (all(c("Product", "Stock") %in% colnames(my_data()))) return(NULL) # make sure your target columns in dataset uploaded
    
    #{if (nrow(my_data()) > 1) ggplot(aes(x=variable, y=value, group=1)) }
    #{if (nrow(my_data()) == 1) ggplot(aes(x=variable, y=value)) }
    
    if(nrow(my_data()) == 1) {
    my_data() %>%
      melt(id.vars = "id") %>%
      ggplot(aes(x=variable, y=value, group=1)) + 
               xlab("") + ylab("") +
      geom_line() + 
      theme_bw() +
      geom_point(size=2) +
        scale_x_discrete(labels = c(
          "Anxious/\nDepressed", "Withdrawn/\nDepressed", "Somatic\nComplaints", "Social\nProblems",
          "Thought\nProblems", "Attention\nProblems", "Rule-Breaking\nBehavior", "Aggressive\nBehavior",
          "Other\nProblems")) +
        ggtitle("CBCL PROFILE — SYNDROME SCALES") + 
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(text=element_text(size=18)) +
        scale_y_continuous(breaks=seq(0, 100, by=5))
      
      } 
    
    else { 
      my_data() %>%
        melt(id.vars = "id") %>%
        ggplot(aes(x=variable, y=value)) + 
        xlab("") + ylab("") +
        theme_bw() +
        geom_boxplot() +
        scale_x_discrete(labels = c(
          "Anxious/\nDepressed", "Withdrawn/\nDepressed", "Somatic\nComplaints", "Social\nProblems",
          "Thought\nProblems", "Attention\nProblems", "Rule-Breaking\nBehavior", "Aggressive\nBehavior",
          "Other\nProblems")) +
        ggtitle("CBCL PROFILE — SYNDROME SCALES") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme(text=element_text(size=18)) +
        scale_y_continuous(breaks=seq(0, 100, by=5))
      
      } 
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(my_data(), file, row.names=FALSE)
      
    })
  
}

# DO NOT ACTUALLY INLCLUDE THESE LINES IN THE FINAL PRODUCT!!!!
# JUST COPY AND PASTE THEM DIRECTLY INTO YOUR CONSOLE!!!

# Run the application 
#shinyApp(ui = ui, server = server)

# Deploy app
#deployApp(appName="cbclR", account="jrcalabrese")

