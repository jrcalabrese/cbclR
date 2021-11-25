#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

ui <-  navbarPage("cbclR by @jrosecalabrese", theme = shinytheme("flatly"), 
                  tabPanel("Calculate", 
                           titlePanel("Upload a file"),
                           sidebarLayout(
                             sidebarPanel(
                               fileInput('file1', 'Choose CSV File',
                                         accept=c('text/csv', 
                                                  'text/comma-separated-values,text/plain', 
                                                  '.csv')),
                               tags$hr(),
                               checkboxInput('header', 'Header', TRUE),
                               radioButtons('sep', 'Separator',
                                            c(Comma=',',
                                              Semicolon=';',
                                              Tab='\t'),
                                            ','),
                               radioButtons('quote', 'Quote',
                                            c(None='',
                                              'Double Quote'='"',
                                              'Single Quote'="'"),
                                            '"'),
                               downloadButton('downloadData', 'Download')
                             ),
                             mainPanel(
                               plotOutput('contents')
                             )
                           )
                  ),
                  tabPanel("Download",
                           titlePanel("Download Sample & Template"),
                           mainPanel(
                             downloadButton('downloadSample', 'Download Sample'),
                             downloadButton('downloadTemplate', 'Download Template')
                           )
                  )
)