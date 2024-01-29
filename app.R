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
  actionButton('start', 'Start')
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
    name <- system("tmux ls | cut -f1 -d:")
    time <- system("tmux ls | cut -f6,7,8 -d' '")
    
    
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
    reactable(mtcars, pagination = FALSE, highlight = TRUE, height = 200, compact = F, fullWidth = FALSE,)
  })
  
  output$stdout <- renderText({
    'bla'
  })
}

shinyApp(ui, server)
