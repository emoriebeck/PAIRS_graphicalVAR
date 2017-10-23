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
  tags$head(tags$style(HTML("
    .shiny-text-output {
                            background-color:#FF0000;
                            }
                            "))),
  
  a(h1("Shiny", span("Idiographic Networks", style = "font-weight: 300"), 
     style = "font-family: 'Source Sans Pro';
        color: #fff; text-align: center;
        background-color:#FF0000;
        padding: 20px"), url = "http://pmdlab.wustl.edu"),
  
  
  br(),
  # Application title
  #titlePanel("Idiographic Personality Networks"),
  
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

edge_colors <- RColorBrewer::brewer.pal(8, "Purples")[c(3,4,6)]
idio_plot_fun <- function(data, subject, wave, type){
  b5_groups <- list(A = c(1,7), E = c(2, 6), C = c(3,8), N = c(4,5,9))
  plot <- 
    qgraph(data, layout = "spring", loop = .7, node.width = 1.85, edge.width = 1, esize = 7,
           title = sprintf("%s Wave %s for S%s", type, wave, subject), label.font = 2, repulsion = .8,
           label.fill.vertical = 1, label.fill.horizontal = 1, edge.color = "black",
           groups = b5_groups, color = rev(t(RColorBrewer::brewer.pal(9, "Purples")[seq(1,7,2)])),
           legend = F, DoNotPlot = TRUE, mar = c(4,4,4,4))
  #change lines to dashed
  plot$graphAttributes$Edges$lty[plot$Edgelist$weight < 0] <- 2
  #change line colors
  plot$graphAttributes$Edges$color <-
    ifelse(abs(plot$Edgelist$weight) <.1, edge_colors[1],
           ifelse(abs(plot$Edgelist$weight) <.2, edge_colors[2], edge_colors[3]))
  dark_colors <- c("#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
  plot$graphAttributes$Nodes$label.color[plot$graphAttributes$Nodes$color %in% dark_colors] <- "white"
  #change variable names
  plot$graphAttributes$Nodes$labels <- gsub("_", "\n", varnames2)
  return(plot)
}


centrality_Plot_fun <- function(data, sub, Type){
  data  %>%
    #filter(SID %in% sub & grepl("trength", measure)) %>%
    gather(key = measure, value = value, -var, -wave, -SID) %>%
    group_by(measure, wave) %>%
    mutate(z = as.numeric(scale(value))) %>%
    arrange(measure, wave) %>%
    ggplot(aes(x = var, y = z, group = wave))+
    geom_line(aes(linetype = wave), color = "black", size = .3) + 
    geom_point(aes(shape = wave), size = 2) + 
    labs(x = NULL, y = "z-score", linetype = "Wave", shape = "Wave",
         title = sprintf("%s Centrality for S%s", Type, sub)) +
    scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1)) + 
    geom_hline(aes(yintercept = 0)) + 
    coord_flip() + 
    facet_grid(SID~measure) + 
    theme_classic()+ 
    theme(axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom")
}

# load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/idiographic_plots.RData")
#load("~/Box Sync/network/PAIRS/PAIRS_graphicalVAR/centralityPlots.RData")
# load_url("https://github.com/emoriebeck/PAIRS-Network-Stability/raw/master/centralityPlots.RData")
load_url("https://github.com/emoriebeck/PAIRS_graphicalVAR/raw/master/app_data.RData")

library(graphicalVAR)
library(tidyverse)
library(gridExtra)


server <- function(input, output, session) {
  observe({
    if(as.character(input$wave) == "1"){
      subs1 <- unique((gVAR_data %>% filter(wave == "1"))$SID)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
    } else {
      subs1 <- unique((gVAR_data %>% filter(wave == "2"))$SID)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
    }
  })
  
  observe({
    if(as.character(input$wave2) == "1"){
      subs2 <- unique((gVAR_data %>% filter(wave == "1"))$SID)
      updateSelectizeInput(session, 'SID2', choices = c("", subs2))
    } else {
      subs2 <- unique((gVAR_data %>% filter(wave == "2"))$SID)
      updateSelectizeInput(session, 'SID2', choices = c("", subs2))
    }
    
  })
  
  observe({
    subs3 <- unique(gVAR_data$SID)
    updateSelectizeInput(session, 'SID3', choices = c("",subs3))
    updateSelectizeInput(session, 'SID4', choices = c("", subs3))
  })
  
  output$gVARPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    print(as.character(input$wave))
    validate(
      need(input$SID, input$SID2, 'Please select 2 Subject IDs'),
      need(input$SID2, 'Please select 2 Subject IDs'))
      if(input$Cor1 == "Temporal"){
        dat <- (gVAR_data %>% filter(SID == input$SID & wave == input$wave))$PDC[[1]]
        plot1 <- idio_plot_fun(dat, input$SID, input$wave, input$Cor1)
        # plot1  <-  plot_beta_w1[[input$SID]]
      } else{
        dat <- (gVAR_data %>% filter(SID == input$SID & wave == input$wave))$PCC[[1]]
        plot1 <- idio_plot_fun(dat, input$SID, input$wave, input$Cor1)
        # plot1  <-  plot_kappa_w1[[input$SID]]
      }
    
    if(input$Cor2 == "Temporal"){
      dat <- (gVAR_data %>% filter(SID == input$SID2 & wave == input$wave2))$PDC[[1]]
      head(dat)
      plot2 <- idio_plot_fun(dat, input$SID2, input$wave2, input$Cor2)
      # plot1  <-  plot_beta_w1[[input$SID]]
    } else{
      dat <- (gVAR_data %>% filter(SID == input$SID2 & wave == input$wave2))$PCC[[1]]
      plot2 <- idio_plot_fun(dat, input$SID2, input$wave2, input$Cor2)
      # plot1  <-  plot_kappa_w1[[input$SID]]
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
      dat <- (gVAR_data %>% filter(SID == input$SID3)) %>% unnest(temp_centrality)
      plot1  <-  centrality_Plot_fun(dat, input$SID3, input$Cor3)
    } else{
      dat <- (gVAR_data %>% filter(SID == input$SID3)) %>% unnest(contemp_centrality)
      plot1  <-  centrality_Plot_fun(dat, input$SID3, input$Cor3)
    }
    
    if(input$Cor3 == "Temporal"){
      dat <- (gVAR_data %>% filter(SID == input$SID4)) %>% unnest(temp_centrality)
      plot2  <-  centrality_Plot_fun(dat, input$SID4, input$Cor4)
    } else{
      dat <- (gVAR_data %>% filter(SID == input$SID4)) %>% unnest(contemp_centrality)
      plot2  <-  centrality_Plot_fun(dat, input$SID4, input$Cor4)
    }
    
    grid.arrange(plot1, plot2, ncol = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

