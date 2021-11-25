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

source("ui.R")

server <- function(input, output) {
  
  getData <- reactive({
    
    inFile <- input$file1
    
    if (is.null(input$file1))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote) %>%
      rowwise() %>%
      mutate(anxdep = sum(c(cbcl14, cbcl29, cbcl30, cbcl31, cbcl32, cbcl33, 
                          cbcl35, cbcl45, cbcl50, cbcl52, cbcl71, cbcl91, cbcl112))) %>%
      mutate(withdep = sum(c(cbcl5, cbcl42, cbcl65, cbcl69, cbcl75, 
                              cbcl102, cbcl103, cbcl111))) %>%
      mutate(soma = sum(c(cbcl47, cbcl49, cbcl51, cbcl54, cbcl56a, 
                        cbcl56b, cbcl56c, cbcl56d, cbcl56e, cbcl56f, 
                        cbcl56g))) %>%
      mutate(social = sum(c(cbcl11, cbcl12, cbcl25, cbcl27, cbcl34, 
                          cbcl36, cbcl38, cbcl48, cbcl62, cbcl64, 
                          cbcl79))) %>%
      mutate(thought = sum(c(cbcl9, cbcl18, cbcl40, cbcl46, cbcl58, 
                           cbcl59, cbcl60, cbcl66, cbcl70, cbcl76, 
                           cbcl83, cbcl84, cbcl85, cbcl92, cbcl100))) %>%
      mutate(attention = sum(c(cbcl1, cbcl4, cbcl8, cbcl10, cbcl13, 
                             cbcl17, cbcl41, cbcl61, cbcl78, cbcl80))) %>%
      mutate(rulebreak = sum(c(cbcl2, cbcl26, cbcl28, cbcl39, cbcl43, 
                             cbcl63, cbcl67, cbcl72, cbcl73, cbcl81, 
                             cbcl82, cbcl90, cbcl96, cbcl99, cbcl101, 
                             cbcl105, cbcl106))) %>%
      mutate(agg = sum(c(cbcl3, cbcl16, cbcl19, cbcl20, cbcl21, 
                       cbcl22, cbcl23, cbcl37, cbcl57, cbcl68, 
                       cbcl86, cbcl87, cbcl88, cbcl89, cbcl94, 
                       cbcl95, cbcl97, cbcl104))) %>%
      mutate(other = sum(c(cbcl6, cbcl7, cbcl15, cbcl24, cbcl44, 
                         cbcl53, cbcl55, cbcl56h, cbcl74, cbcl77, 
                         cbcl93, cbcl98, cbcl107, cbcl108, cbcl109, 
                         cbcl110, cbcl113))) %>%
      select(id, anxdep, withdep, soma, social, thought, attention, rulebreak, agg, other) %>% as.data.frame()
    
    #hello_plot <- df %>%
      #ggplot(aes(x=anxdep, y=withdep)) + 
      #geom_bar(stat="identity")
    
  })
  
  #output$contents <- renderPlot(
    #hello_plot()	
  #)
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      
      write.csv(getData(), file, row.names=FALSE)
      
    })
  
}

# DO NOT ACTUALLY INLCLUDE THESE LINES IN THE FINAL PRODUCT!!!!
# JUST COPY AND PASTE THEM DIRECTLY INTO YOUR CONSOLE!!!

# Run the application 
#shinyApp(ui = ui, server = server)

# Deploy app
#deployApp(appName="cbclR", account="jrcalabrese")

