library(shiny)
library(shinydashboard)
library(waiter)
library(plotly)
library(stringr)


#### load template data for first Tab
spectra <- vroom::vroom('spectra.txt')
# stages 
stages <- vroom::vroom('stages.txt')

# change upload max size
options(shiny.maxRequestSize = 5*1024*1024^2)

#################### Chat parameters ###########################################
# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){ vars$chat <- readRDS("chat.Rds")}

# Get the prefix for the line to be added to the chat window. Usually a newline character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

############ UI     ######################
ui <- dashboardPage(
  dashboardHeader(title='Scoring App'),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Preview of app output", tabName = "spe_tra", icon = icon("search")),
      menuItem("Autom. sleep scoring", tabName = "upload", icon = icon("brain")),
      menuItem("Chat / Discuss / Report", tabName = "chat-room", icon = icon("comments"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("spe_tra",
              box(plotlyOutput("plot_preview"), width = 12)
      ),
      tabItem("upload",
              box(fileInput("edf_file1", "Upload EDF or BDF File", multiple = FALSE, accept = c(".edf",".EDF", ".REC", ".rec", ".BDF", ".bdf")),
                  strong("After uploading your EDF(+) or BDF(+) file choose EEG channels of interest."), br(),
                  p("Choose a main bipolar derivation (ex. C3-A2), 2 ocular channels and 1 EMG channel. 
                    Process of automatic sleep scoring can take a while, since data are downsampled, filtered, Welch’s power spectral density is computed 
                    (5 second sliding window 50% overlap) and passed from R to python
                    (still this process should take less than a minute). Epoch length is set to 20 seconds for the moment, but other settings will
                    soon be available"), width = 6),
              box(div(id="placeholder1"), width = 3),
              box(div(id="placeholder2"),uiOutput("stage_dl_button"), br(), uiOutput("spectra_dl_button"), width = 3),
              box(useAttendant(),attendantBar("progress-bar"), plotlyOutput("scored_plot"), width = 12)
      ),
      tabItem("chat-room", h3("Chat room:"), 
              box(uiOutput("chat"), width = 8, height = 500),
              box(textInput("user", "Change you username:", value=""), h5("Connected Users right now:"), uiOutput("userList"), textInput("entry", "Input your message here:"), actionButton("send", "Send"), width = 4),
      )
    )
  )
)

################   SERVER ##############################################

server <- function(input, output, session) {
  
  ############ Tab preview   ####################################################
  output$plot_preview <- renderPlotly({
    
    fig <- plot_ly(spectra, x = ~Epoch, y = ~Freq, z = ~Power, colors = "Spectral") %>%  
      add_heatmap() %>% colorbar(title = "-Log(Power)") %>%
      plotly::layout(yaxis=list(title='Frequency [Hz]'), xaxis=list(title=''))
    
    fig2 <- plot_ly(stages, x = ~Epoch, name = 'Autom. scored') %>%
      add_ribbons(ymin = ~Stages - 0.5, ymax = ~Stages + 0.5, color =  I("black")) %>%
      plotly::layout(title='Preview of app output',
                     yaxis = list(title='Stages',
                                  range=c(-1.5,4),
                                  ticktext = list("REM", "Wake", "N1", "N2", "SWS"),
                                  tickvals = list(-1, 0, 1, 2, 3),
                                  tickmode = "array"
                     ), xaxis = list(title = 'Epoch\nTo score your own recording switch to Tab "Autom. sleep scoring"')
      )
    subplot(fig, fig2 , nrows = 2, titleX = TRUE, titleY = TRUE)
  })
  
  
  ######## Tab Autom. sleep scoring  #########################################
  ch <- eventReactive(input$edf_file1, {
    edfReader::readEdfHeader(input$edf_file1$datapath)$sHeaders$label
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder1",
      where = "afterEnd",
      actionButton("score_button", "Start scoring!")
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder1",
      where = "afterEnd",
      ui = shiny::selectInput("s_emg", "Select EMG channel:",ch())
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder2",
      where = "afterEnd",
      ui = shiny::selectInput("s_roc", "Select ROC channel:",ch())
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder2",
      where = "afterEnd",
      ui = shiny::selectInput("s_loc", "Select LOC channel:",ch())
    )
  })
  
  observeEvent(input$edf_file1, {
    insertUI(
      selector = "#placeholder1",
      where = "afterEnd",
      ui = shiny::selectInput("s_c3a2", "Select main (eg. C3A2) channel:",ch())
    )
  })
  
  att <- Attendant$new("progress-bar", hide_on_max = TRUE) # progress bar
  
  observeEvent(input$score_button, {
    source('./function_first_step.r')   # load function
    score(input$edf_file1$datapath, input$s_c3a2, input$s_loc, input$s_roc, input$s_emg, att$set)
  })
  
  f_scored <- eventReactive(input$score_button,{
    att$done() 
    R.matlab::readMat('./score_sleep/pred/01.mat.mat')
  })
  
  
  output$scored_plot <- renderPlotly({
    fig_spectra <- plot_ly(f_scored()$Pspec %>% tidyr::as_tibble() %>%
                             mutate(Freq=1:n()/5-0.2) %>%
                             tidyr::pivot_longer(!Freq, names_to = 'Epoch', values_to = 'Power') %>%
                             mutate(Epoch=readr::parse_number(Epoch), Power=-round(log10(Power),2)) %>%
                             dplyr::filter(Freq<=30), x = ~Epoch, y = ~Freq, z = ~Power, colors = "Spectral") %>%  
      add_heatmap() %>% colorbar(title = "-Log Power") %>% 
      plotly::layout(yaxis=list(title='Frequency [Hz]'), xaxis = list(title = ''))
    
    fig_hypno <- plot_ly(f_scored()$y. %>% t() %>% tidyr::as_tibble() %>% rename('Stages'='V1') %>%
                           mutate(Epoch=1:n(), Stages=as.numeric(Stages),Stages=if_else(Stages==4, -1, Stages)), 
                         x = ~Epoch, name = 'Hypnogramm') %>%
      add_ribbons(ymin = ~Stages - 0.5, ymax = ~Stages + 0.5, color = I("black")) %>%
      plotly::layout(
        yaxis = list(title='Stages',
                     range=c(-1.5,4),
                     ticktext = list("REM", "Wake", "N1", "N2", "SWS"),
                     tickvals = list(-1, 0, 1, 2, 3),
                     tickmode = "array"
        ), xaxis = list(title = 'Epoch')
      )
    
    subplot(fig_spectra, fig_hypno, nrows = 2, titleX = TRUE, titleY=TRUE)
  })
  
  
  output$stage_dl_button <- renderUI({
    req(input$score_button)
    downloadButton('download_item', label = 'Download stages') })
  
  
  output$download_item <- downloadHandler(
    filename = function() {paste("stages-", Sys.Date(), ".csv", sep="")},
    content = function(file) {write.csv(f_scored()$y. %>% t() %>% tidyr::as_tibble() %>% rename('Stages'='V1') %>%
                                          mutate(Epoch=1:n(), Stages=as.numeric(Stages),Stages=if_else(Stages==4, -1, Stages)), file)
    }
  )
  
  output$spectra_dl_button <- renderUI({
    req(input$score_button)
    downloadButton('download_item2', label = 'Download spectra') })
  
  
  output$download_item2 <- downloadHandler(
    filename = function() {paste("spectra-", Sys.Date(), "-NonLog.csv", sep="")},
    content = function(file) {write.csv(f_scored()$Pspec %>% tidyr::as_tibble() %>% rename_with(~ gsub("V", "Epoch_", .x)) %>%
                                          mutate(Freq=1:n()/5-0.2) %>% dplyr::filter(Freq<=30), file)
    }
  )
  
  session$onSessionEnded(function() {
    system(paste("rm -f",'./score_sleep/pred/01.mat.mat'))
    system(paste("rm -f",'../mat/01.mat'))
  })
  
  
  ###############    Tab Chat-room     #########################################################################
  # Create a spot for reactive variables specific to this particular session
  sessionVars <- reactiveValues(username = "")
  
  # Track whether or not this session has been initialized. We'll use this to assign a username to unininitialized sessions.
  init <- FALSE
  
  # When a session is ended, remove the user
  session$onSessionEnded(function() {
    isolate({
      vars$users <- vars$users[vars$users != sessionVars$username]
    })
  })
  
  observe({   # Observer to handle changes to the username
    input$user # We want a reactive dependency on this variable, so we'll just list it here.
    if (!init){
      sessionVars$username <- paste0("User", round(runif(1, 10000, 99999))) # Seed initial username
      init <<- TRUE
    } else{
      isolate({
        if (input$user == sessionVars$username || input$user == ""){  # A previous username was already given
          return() # No change. Just return.
        }
        vars$users <- vars$users[vars$users != sessionVars$username]  # Updating username. First, remove the old one
        sessionVars$username <- input$user                            # Now update with the new one
      })
    }
    isolate(vars$users <- c(vars$users, sessionVars$username))     # Add this user to the global list of users
  })
  
  observe({
    updateTextInput(session, "user", value=sessionVars$username)     # Keep the username updated with whatever sanitized/assigned username we have
  })
  
  output$userList <- renderUI({
    tagList(tags$ul( lapply(vars$users, function(user){ # Keep the list of connected users updated
      return(tags$li(user))
    })))
  })
  
  observe({
    if(input$send < 1){   # Listen for input$send changes (i.e. when the button is clicked)
      return()  # The code must be initializing, b/c the button hasn't been clicked yet.
    }
    isolate({ 
      vars$chat <<- c(vars$chat, paste0(linePrefix(), tags$span(class="username", tags$abbr(title=Sys.time(), sessionVars$username)), ": ", tagList(input$entry))) # Add the current entry to the chat log.
    })
    updateTextInput(session, "entry", value="")     # Clear out the text entry field.
  })
  
  output$chat <- renderUI({   # Dynamically create the UI for the chat window.
    if (length(vars$chat) > 500){
      vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]       # Too long, use only the most recent 500 lines
    }
    saveRDS(vars$chat, "chat.Rds")     # Save the chat object so we can restore it later if needed.
    HTML(vars$chat)     # Pass the chat log through as HTML
  })
  
}

shinyApp(ui, server)