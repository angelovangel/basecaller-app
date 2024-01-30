library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(tibble)
library(stringr)
library(dplyr)
library(shinyjs)
library(processx)
library(reactable)
library(lubridate)

sidebar <- sidebar(
  title = "Controls",
  selectizeInput(
    "model", "Select dorado model",
    choices = c('fast', 'hac', 'sup')
  ),
  numericInput("seconds", "Sleep secs", 3),
  actionButton('start', 'Start dorado (new session)'),
  actionButton('close', 'Close session'),
  actionButton('ctrlc', 'Send ctrl-c')
)

ui <- page_navbar(
  useShinyjs(),
  fillable = T,
  title = 'ONT basecaller app',
  theme = bs_theme(font_scale = 0.9, bootswatch = 'yeti', primary = '#2C3E50'),
  sidebar = sidebar,
  nav_panel(title = "",
  card(max_height = '250px',
  reactableOutput('tmux_table')
  ),
  card(max_height = '400px',
  verbatimTextOutput('stdout')
  )
  )
)

server <- function(input, output, session) {
  # reactives
  
  # track tmux sessions
  # empty df for init
  empty_df <- data.frame(
    session_id = NA,
    started = NA,
    runtime = NA,
    attached = NA,
    session_path = NA
  )
  
  tmux_sessions <- reactive({
    invalidateLater(2000, session)
    tmuxinfo <- system2("bin/helper.sh", stdout = TRUE, stderr = TRUE)
    
    if (any(str_detect(tmuxinfo, 'no server|error'))) {
      data.frame(
        session_id = NA,
        started = NA,
        runtime = NA,
        attached = NA,
        session_path = NA
      )
    } else {
      data.frame(
        session_id = str_split_i(tmuxinfo, " ", 2),
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        runtime = NA,
        attached = str_split_i(tmuxinfo, " ", 3),
        session_path = str_split_i(tmuxinfo, " ", 4)
      ) %>%
       mutate(
         runtime = difftime(now(), started, units = 'hours'),
         attached = if_else(as.numeric(attached) == 1, 'yes', 'no')
       ) 
    }
  })
  
  selected <- reactive({
    getReactableState('tmux_table', 'selected')
  })
  
  # observers
  observeEvent(input$start, {
    withCallingHandlers({
      p <- processx::run(
        'cowsay', args = c(as.character(input$seconds)),
        stdout_line_callback = function(line, proc) { message(line) }, 
        #stdout_callback = cb_count,
        stderr_to_stdout = TRUE, 
        error_on_status = FALSE
      )
    },
    message = function(m) {
      shinyjs::html(id = "stdout", html = m$message, add = TRUE);
      runjs("document.getElementById('stdout').scrollTo(0,1e9);") # scroll the page to bottom with each message, 1e9 is just a big number
    })
  })
  
  # close session
  observeEvent(input$close, {
    session_selected <- tmux_sessions()[selected(), ]$session_id
    args <- paste0('kill-session -t ', session_selected)
    system2('tmux', args = args)  
  })
  
  # send ctrl-c
  observeEvent(input$ctrlc, {
    session_selected <- tmux_sessions()[selected(), ]$session_id
    args <- paste0('send-keys -t ', session_selected, ' C-c')
    system2('tmux', args = args)
  })
  
  # outputs
  output$tmux_table <- renderReactable({
    reactable(
      empty_df,
      #tmux_sessions(), 
      pagination = FALSE, highlight = TRUE, height = 200, compact = T, 
      fullWidth = T, selection = 'single', onClick = 'select',
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
      ),
      columns = list(
        started = colDef(format = colFormat(datetime = T, locales = 'en-GB')),
        runtime = colDef(format = colFormat(suffix = ' h', digits = 2))
      )
    )
  })
  
  observe({
    updateReactable('tmux_table', data = tmux_sessions(), selected = selected())
  })
  
  output$stdout <- renderText({
    selected()
  })
}

shinyApp(ui, server)
