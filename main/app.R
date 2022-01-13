library(shiny)
library(shinydashboard)
library(waiter)
library(plotly)

# change upload max size
options(shiny.maxRequestSize = 5*1024*1024^2)

# load script
source('./function_first_step.r')

waiting_screen <- tagList(
  spin_flower(),
  h4("Take a coffee break. This may take some time (around a minute).")
) 

ui <- dashboardPage(
  dashboardHeader(title='Scoring app'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Autom. sleep scoring", tabName = "upload", icon = icon("tree"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("upload",
              box(fileInput("edf_file1", "Upload EDF File", multiple = FALSE, accept = c(".txt",".edf",".EDF", ".REC")),
                  strong("Choose EEG channels of interest."), br(),
                  p("Choose a main bipolar derivation (ex. C3A2), 2 ocular channels and 1 EMG channel"), width = 6),
              box(div(id="placeholder1"), width = 3),
              box(div(id="placeholder2"),uiOutput("stage_dl_button"), br(), uiOutput("spectra_dl_button"), useWaiter(), width = 3),
              box(plotlyOutput("scored_plot"), width = 12)
      )
    )
  )
)


server <- function(input, output, session) {
  
  ch <- eventReactive(input$edf_file1, {
    edfReader::readEdfHeader(input$edf_file1$datapath)$sHeaders$label
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder1",
      where = "afterEnd",
      actionButton("score_button", "Score!")
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder1",
      where = "afterEnd",
      ui = shiny::selectInput("s_emg", "Choose EMG:",ch())
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder2",
      where = "afterEnd",
      ui = shiny::selectInput("s_roc", "Choose ROC:",ch())
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder2",
      where = "afterEnd",
      ui = shiny::selectInput("s_loc", "Choose LOC:",ch())
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder1",
      where = "afterEnd",
      ui = shiny::selectInput("s_c3a2", "Choose C3A2:",ch())
    )
  })
  
  observeEvent(input$score_button, {
    waiter_show(html = waiting_screen, color = "black")
    score(input$edf_file1$datapath, input$s_c3a2, input$s_loc, input$s_roc, input$s_emg)
    waiter_hide()
  })
  
  f_scored <- eventReactive(input$score_button,{
    R.matlab::readMat('./pred/01.mat.mat')
  })
  
  
  output$scored_plot <- renderPlotly({
  fig_spectra <- plot_ly(f_scored()$Pspec %>% as_tibble() %>%
                           mutate(Freq=1:n()/5-0.2) %>%
                           tidyr::pivot_longer(!Freq, names_to = 'Epoch', values_to = 'Power') %>%
                           mutate(Epoch=readr::parse_number(Epoch), Power=-round(log10(Power),2)) %>%
                           dplyr::filter(Freq<=30), x = ~Epoch, y = ~Freq, z = ~Power, colors = "Spectral") %>%  
    add_heatmap() %>% colorbar(title = "-Log Power") %>% plotly::layout(xaxis = list(title = ''))
  
  fig_hypno <- plot_ly(f_scored()$y. %>% t() %>% as_tibble() %>% rename('Stages'='V1') %>%
                         mutate(Epoch=1:n(), Stages=as.numeric(Stages),Stages=if_else(Stages==4, -1, Stages)), 
                       x = ~Epoch, name = 'Hypnogramm') %>%
    add_ribbons(ymin = ~Stages - 0.5, ymax = ~Stages + 0.5, color = I("black")) %>%
    plotly::layout(
      yaxis = list(
        range=c(-1.5,4),
        ticktext = list("REM", "Wake", "N1", "N2", "SWS"),
        tickvals = list(-1, 0, 1, 2, 3),
        tickmode = "array"
      ), xaxis = list(title = '')
    )
  
  subplot(fig_spectra, fig_hypno, nrows = 2, titleX = TRUE)
  })
  
  
  output$stage_dl_button <- renderUI({
    req(input$score_button)
    downloadButton('download_item', label = 'Download stages') })
  
  
  output$download_item <- downloadHandler(
    filename = function() {paste("stages-", Sys.Date(), ".csv", sep="")},
    content = function(file) {write.csv(f_scored()$y. %>% t() %>% as_tibble() %>% rename('Stages'='V1') %>%
                                          mutate(Epoch=1:n(), Stages=as.numeric(Stages),Stages=if_else(Stages==4, -1, Stages)), file)
      }
  )
  
  
  output$spectra_dl_button <- renderUI({
    req(input$score_button)
    downloadButton('download_item2', label = 'Download spectra') })
  
  
  output$download_item2 <- downloadHandler(
    filename = function() {paste("spectra-", Sys.Date(), ".csv", sep="")},
    content = function(file) {write.csv(f_scored()$Pspec %>% as_tibble() %>%
                                          mutate(Freq=1:n()/5-0.2) %>% dplyr::filter(Freq<=30), file)
    }
  )
  
  session$onSessionEnded(function() {
    system(paste("rm -f",'./pred/01.mat.mat'))
    system(paste("rm -f",'../mat/01.mat'))
  })
}

shinyApp(ui, server)


