
# Define server logic required to draw a histogram

load_url <- function (url, ..., sha1 = NULL) {
  # based very closely on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  load(temp_file, envir = .GlobalEnv)
}

load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/idiographic_plots.RData")
#load("~/Box Sync/network/PAIRS/PAIRS_graphicalVAR/centralityPlots.RData")
load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/centralityPlots.RData")

library(graphicalVAR)
library(tidyverse)
library(gridExtra)


server <- function(input, output, session) {
observe({
  if(as.character(input$wave) == "1"){
      subs1 <- names(plot_kappa_w1)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
    } else {
      subs1 <- names(plot_kappa_w2)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
    }
})

observe({
  if(as.character(input$wave2) == "1"){
    subs2 <- names(plot_kappa_w1)
    updateSelectizeInput(session, 'SID2', choices = c("", subs2))
  } else {
    subs2 <- names(plot_kappa_w2)
    updateSelectizeInput(session, 'SID2', choices = c("", subs2))
  }
    
})

observe({
  subs3 <- names(centralityPCC)
  updateSelectizeInput(session, 'SID3', choices = c("",subs3))
  updateSelectizeInput(session, 'SID4', choices = c("", subs3))
})
    
    output$gVARPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      print(as.character(input$wave))
      validate(
        need(input$SID, input$SID2, 'Please select 2 Subject IDs'),
        need(input$SID2, 'Please select 2 Subject IDs'))
      if(as.character(input$wave) == "1") {
        if(input$Cor1 == "Temporal"){
          plot1  <-  plot_beta_w1[[input$SID]]
        } else{
          plot1  <-  plot_kappa_w1[[input$SID]]
        }
      } else {
        if(input$Cor1 == "Temporal"){
          plot1  <-  plot_beta_w2[[input$SID]]
        } else{
          plot1  <-  plot_kappa_w2[[input$SID]]
        }
      }
      
      if(as.character(input$wave2) == "1") {
        if(input$Cor2 == "Temporal"){
          plot2  <-  plot_beta_w1[[input$SID2]]
        } else{
          plot2  <-  plot_kappa_w1[[input$SID2]]
        }
      } else {
        if(input$Cor2 == "Temporal"){
          plot2  <-  plot_beta_w2[[input$SID2]]
        } else{
          plot2  <-  plot_kappa_w2[[input$SID2]]
        }
      }
      
    
      # draw the histogram with the specified number of bins
      if(!("" %in% input)){
        par(mfrow = c(1,2))
        plot(plot1)
        plot(plot2)
      }
    })
    
    output$centrality <- renderPlot({
      # generate bins based on input$bins from ui.R
      validate(
        need(input$SID3, input$SID4, 'Please select 2 Subject IDs'),
        need(input$SID4, 'Please select 2 Subject IDs'))
        if(input$Cor3 == "Temporal"){
          plot1  <-  centralityPDC[[input$SID3]]
        } else{
          plot1  <-  centralityPCC[[input$SID3]]
        }
      
        if(input$Cor4 == "Temporal"){
          plot2  <-  centralityPDC[[input$SID4]]
        } else{
          plot2  <-  centralityPCC[[input$SID4]]
        }
      
        grid.arrange(plot1, plot2, ncol = 2)
    })

}