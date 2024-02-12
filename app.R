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
library(shinyFiles)
library(shinyWidgets)
library(shinybusy)
library(digest)

# global
barcoding_kits <- read.csv('data/kits.csv')$kit

# to handle tmux and nvidia-smi executables availability on macos and linux
is_bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}


sidebar <- sidebar(
  title = "Controls",
  selectizeInput('gpus', 'GPUs on machine', choices = c(1:4), selected = 4, multiple = F),
  checkboxInput('duplex', 'Duplex basecalling', value = F),
  selectizeInput(
    "model", "Select dorado model",
    choices = c('fast', 'hac', 'sup')
  ),
  shinyDirButton("pod5", "Select pod5 folder", title ='Please select a folder with signal data', multiple = F),
  checkboxInput('recursive', 'Search recursively'),
  checkboxInput('barcoded', 'Barcoded run'),
  uiOutput('minknow_output'),
  uiOutput('kits'),
  #tags$hr(),
  textInput('session_name', 'Name for new session (optional)', value = 'tgs'),
  actionButton('start', 'Start dorado (new session)'),
  actionButton('show_session', 'Show session pane'),
  actionButton('ctrlc', 'Send ctrl-c to session'),
  actionButton('kill', 'Kill session'),
)

ui <- page_navbar(
  tags$head(
    tags$style(
      ".progress {
          transform: rotate(180deg);
          background: linear-gradient(to left, yellow 0%, red 100%);
        //background-color: orange;
        //opacity: 0.9;
        //color: white;
      }
      .progress-bar {
        background: #EAECEE;
      }
      ")
  ),

  useShinyjs(),
  fillable = T,
  title = 'ONT basecaller app',
  theme = bs_theme(font_scale = 0.9, bootswatch = 'yeti', primary = '#2C3E50'),
  sidebar = sidebar,
  nav_panel(
    title = "",
    uiOutput('progressbars'),
    verbatimTextOutput('pod5_selected'),
    card(max_height = '250px',
    reactableOutput('tmux_table')
    ),
    card(max_height = '400px',
    verbatimTextOutput('stdout')
    )
  )
)

server <- function(input, output, session) {
  if (!is_bin_on_path('tmux')){
    oldpath <- Sys.getenv('PATH')
    Sys.setenv(PATH = paste(oldpath, '/opt/homebrew/bin', sep = ":"))
  }
  
  # shiny files
  default_path <- Sys.getenv('DEFAULT_PATH')
  volumes <- c(ont_data = default_path, getVolumes()())
  default_path <- Sys.getenv('DEFAULT_PATH')
  shinyDirChoose(
    input, "pod5", 
    roots = volumes, #defaultPath = default_path,
    session = session, allowDirCreate = FALSE
    )
  
  # reactives
  if (is_bin_on_path('nvidia-smi')) {
    cmd <- 'nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits'
  } else {
    cmd <- 'tail -n 4 data/smi.txt'
  }
  
  newLines <- reactive({
    invalidateLater(1000, session)
    readLines(pipe(cmd)) %>% as.numeric() %>% tail(as.numeric(input$gpus))
  })
  
  # track tmux sessions
  # empty df for init
  empty_df <- data.frame(
    session_id = NA,
    started = NA,
    runtime = NA,
    command = NA,
    active = NA,
    attached = NA,
    session_path = NA
  )
  
  tmux_sessions <- reactive({
    invalidateLater(2000, session)
    oldw <- getOption("warn")
    options(warn = -1)
    tmuxinfo <- system2("bin/helper.sh", stdout = TRUE, stderr = TRUE)
    options(warn = oldw)
    
    if (any(str_detect(tmuxinfo, 'no server|error'))) {
      empty_df
    } else {
      data.frame(
        session_id = str_split_i(tmuxinfo, " ", 2),
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        runtime = NA,
        command = str_split_i(tmuxinfo, " ", 5),
        active = str_split_i(tmuxinfo, " ", 6),
        attached = str_split_i(tmuxinfo, " ", 3),
        session_path = str_split_i(tmuxinfo, " ", 4)
      ) %>%
       mutate(
         runtime = difftime(now(), started, units = 'hours'),
         attached = if_else(as.numeric(attached) == 1, 'yes', 'no')
      ) %>%
       arrange(started)
    }
  })
  
  selected <- reactive({
    getReactableState('tmux_table', 'selected')
  })
  
  # observers
  
  # make progress bars
  output$progressbars <- renderUI({
    gpuvalues <- newLines()
    fluidRow(
     lapply(1:input$gpus, function(x, status) {
       if (gpuvalues[x] < 33) {
         status <- 'success'
       } else if (gpuvalues[x] >=33 & gpuvalues[x] < 66) {
         status <- 'warning'
       } else {
         status <- 'danger'
       }
       column(
         width = 12/as.numeric(input$gpus), 
         progressBar(id = paste0('pb', x), value = 100 - gpuvalues[x], display_pct = F, title = paste0('GPU', x)))
     })
    )
  })
  
  
  # start basecalling
  observeEvent(input$start, {
    
    new_session_name <- paste0(digest::digest(runif(1), algo = 'crc32'), '-', input$session_name)
    pod5dir <- parseDirPath(volumes, input$pod5)
    # launch new session
    
    args1 <- c('new', '-d', '-s', new_session_name)
    system2('tmux', args = args1)
    
    rec <- ifelse(input$recursive, '-r', '')
    folders <- ifelse(input$folder_output, '-f', '')
    kit <- ifelse(input$barcoded, paste0('-k', input$kit), '')
    
    # execute dorado in the new session
    string <- paste(
      dorado_script(), 'Space', '-p', 'Space', pod5dir, 'Space',  
      '-m', 'Space', input$model, 'Space', rec, 'Space', kit, 'Space', folders, sep = ' '
      )
    args2 <- c('send-keys', '-t', new_session_name, string, 'C-m')
    system2('tmux', args = args2)
    notify_success(text = paste0('Started session ', new_session_name), timeout = 2000, position = 'center-bottom')
  })
  
  # attach
   observeEvent(input$show_session, {
   #observe({
    session_selected <- tmux_sessions()[selected(), ]$session_id
    
    withCallingHandlers({
      shinyjs::html(id = "stdout", "")
      args <- c('capture-pane', '-S', '-', '-E', '-', '-pt', session_selected)

      p <- processx::run(
        'tmux', args = args,
        #stdout_callback = function(line, proc) {message(line)},
        stdout_line_callback = function(line, proc) {message(line)},
        stderr_to_stdout = TRUE,
        error_on_status = FALSE
      )
    },
    message = function(m) {
      shinyjs::html(id = "stdout", html = m$message, add = T);
      #runjs("document.getElementById('stdout').parentElement.scrollTo(0,1e9);")
      runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
      }
    )
  })
  
  # close session
  observeEvent(input$kill, {
    session_selected <- tmux_sessions()[selected(), ]$session_id
    args <- paste0('kill-session -t ', session_selected)
    if (!is.null(selected())) {
      system2('tmux', args = args)
      notify_success(text = paste0('Session ', session_selected, ' killed!'), timeout = 2000, position = 'center-bottom')
    } else{
      notify_failure('Select session first!', timeout = 2000, position = 'center-bottom')
    }  
  })
  
  # send ctrl-c
  observeEvent(input$ctrlc, {
    session_selected <- tmux_sessions()[selected(), ]$session_id
    args <- paste0('send-keys -t ', session_selected, ' C-c')
    if (!is.null(selected())) {
      system2('tmux', args = args)
      notify_success(text = paste0('Ctrl-C sent to session ', session_selected), timeout = 2000, position = 'center-bottom')
    } else {
      notify_failure('Select session first!', timeout = 2000, position = 'center-bottom')
    }
    
  })
  
  observe({
    if (input$barcoded) {
      updateCheckboxInput('recursive', value = T, session = session)
    } else {
      updateCheckboxInput('recursive', value = F, session = session)
    }
  })
  
  dorado_script <- reactiveVal()
  observe({
    if (input$duplex) {
      updateSelectizeInput('model', choices = c('hac', 'sup'), session = session)
      dorado_script('ont-duplex-basecall.sh')
    } else {
      updateSelectizeInput('model', choices = c('fast', 'hac', 'sup'), session = session)
      dorado_script('ont-basecall.sh')
    }
  })
  
  # outputs
  # show kits if barcoded run
  output$kits <- renderUI({
    if (input$barcoded) {
      selectizeInput('kit', 'Barcoding kit', choices = barcoding_kits, selected = 'SQK-RBK114-96')
    } else {
      NULL
    }
  })
  
  output$minknow_output <- renderUI({
    if (input$barcoded) {
      checkboxInput('folder_output', 'Output in folders', value = TRUE)
    }
  })
  output$tmux_table <- renderReactable({
    reactable(
      empty_df,
      #tmux_sessions(), 
      pagination = FALSE, highlight = TRUE, height = 200, compact = T, 
      fullWidth = T, selection = 'single', onClick = 'select', defaultSelected = 1,
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
  
  # handle pod5 directory
  observe({
    if (is.integer(input$pod5)) {
      shinyjs::disable('start')
    } else {
      pod5dir <- parseDirPath(volumes, input$pod5)
      pod5files <- length(list.files(pod5dir, pattern = '*.pod5', recursive = input$recursive))
      if (pod5files > 0) {
        shinyjs::enable('start')
      }
    }
  })
  
  output$stdout <- renderText({
    if (is.integer(input$pod5)) {
      "No directory has been selected"
    } else {
      pod5dir <- parseDirPath(volumes, input$pod5)
      pod5files <- length(list.files(pod5dir, pattern = '*.pod5', recursive = input$recursive))
      paste0(
        'Selected pod5 directory: ', pod5dir, '\n',
        pod5files, ' pod5 files found')
    }
  })
  
  output$pod5_selected <- renderText({
    if (is.integer(input$pod5)) {
      "No directory has been selected"
    } else {
      pod5dir <- parseDirPath(volumes, input$pod5)
      pod5dir
    }
  })

}

shinyApp(ui, server)
