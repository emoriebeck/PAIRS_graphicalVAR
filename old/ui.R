#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Idiographic Personality Networks"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("wave",
                                "Wave:",
                                choices = c("1", "2")),
                 selectizeInput("SID",
                                "Subject ID #1:",
                                choices = ""),
                 selectizeInput("Cor1",
                                "Select Correlation Type",
                                choices = c("Temporal", "Contemporaneous")),
                 selectizeInput("wave2",
                                "Wave:",
                                choices = c("1", "2")),
                 selectizeInput("SID2",
                                "Subject ID #2:",
                                choices = ""),
                 selectizeInput("Cor2",
                                "Select Correlation Type",
                                choices = c("Temporal", "Contemporaneous"))
               ), 
               mainPanel(
                 plotOutput("gVARPlot")
               ))),
    tabPanel("Centrality",  
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("SID3",
                                "Subject ID #1:",
                                choices = ""),
                 selectizeInput("Cor3",
                                "Select Correlation Type",
                                choices = c("Temporal", "Contemporaneous")),
                 selectizeInput("SID4",
                                "Subject ID #2:",
                                choices = ""),
                 selectizeInput("Cor4",
                                "Select Correlation Type",
                                choices = c("Temporal", "Contemporaneous"))
               ), 
               mainPanel(
                 plotOutput("centrality")
               )))
  )
    
    # Show a plot of the generated distribution
    
  )

