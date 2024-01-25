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
  numericInput("seconds", "Sleep secs", 3)
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
  verbatimTextOutput('stdout')
  )
)

server <- function(input, output, session) {
  # outputs
  output$tmux_sessions <- renderReactable({
    reactable(mtcars, pagination = FALSE, highlight = TRUE, height = 200, compact = F, fullWidth = FALSE,)
  })
  
  output$stdout <- renderText({
    'bla'
  })
}

shinyApp(ui, server)
