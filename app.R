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

sidebar <- sidebar(
  title = "Controls",
  selectizeInput(
    "model", "Select dorado model",
    choices = c('fast', 'hac', 'sup')
  ),
  numericInput("seconds", "Sleep secs", 3),
  actionButton('start', 'Start dorado'),
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
  reactableOutput('tmux_sessions')
  ),
  card(max_height = '400px',
  verbatimTextOutput('stdout')
  )
  )
)

server <- function(input, output, session) {
  # reactives
  
  # track tmux sessions
  tmux_sessions <- reactive({
    invalidateLater(2000, session)
    tmuxinfo <- system2("bin/helper.sh", stdout = TRUE, stderr = TRUE)
    
    if (str_detect(tmuxinfo[[1]], 'no server')) {
      data.frame(
        session_id = NA,
        started = NA,
        attached = NA,
        session_path = NA
      )
    } else {
      data.frame(
        session_id = str_split_i(tmuxinfo, " ", 2),
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        attached = str_split_i(tmuxinfo, " ", 3),
        session_path = str_split_i(tmuxinfo, " ", 4)
      )
    }
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
  # outputs
  output$tmux_sessions <- renderReactable({
    reactable(
      tmux_sessions(), 
      pagination = FALSE, highlight = TRUE, height = 200, compact = T, fullWidth = T,
      columns = list(
        started = colDef(format = colFormat(datetime = T, locales = 'en-GB'))
      )
      )
  })
  
  output$stdout <- renderText({
    'bla'
  })
}

shinyApp(ui, server)
