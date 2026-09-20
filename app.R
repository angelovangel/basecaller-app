library(shiny)
library(shinyWidgets)
library(shinymanager)
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
library(hover)

# global
barcoding_kits <- read.csv('data/kits.csv')$kit

# to handle tmux and nvidia-smi executables availability on macos and linux
is_bin_on_path = function(bin) {
  exit_code = suppressWarnings(system2("command", args = c("-v", bin), stdout = FALSE))
  return(exit_code == 0)
}

# extract the base basecalling speed (fast/hac/sup) from either a plain
# selection ('fast'/'hac'/'sup') or a full versioned dorado model name
# (e.g. 'dna_r10.4.1_e8.2_400bps_hac@v4.2.0')
model_base <- function(m) {
  if (is.null(m) || length(m) == 0) return(NA_character_)
  if (m %in% c('fast', 'hac', 'sup')) return(m)
  if (grepl('_fast@', m)) return('fast')
  if (grepl('_hac@', m)) return('hac')
  if (grepl('_sup@', m)) return('sup')
  NA_character_
}


sidebar <- sidebar(
  title = "Controls",
  tags$div(
    class = 'sidebar-toolbar',
    hover_action_button('start', 'Start run', button_animation = 'overline-reveal'),
    hover_action_button('show_session', 'Show session', button_animation = 'overline-reveal'),
    hover_action_button('ctrlc', 'Send Ctrl-C', button_animation = 'overline-reveal'),
    hover_action_button(inputId = 'kill', label = 'Kill session', button_animation = 'overline-reveal'),
    hover_action_button('show_models', 'Show available models', button_animation = 'overline-reveal'),
    hover_action_button('show_gpu', 'Show GPU info', button_animation = 'overline-reveal')
  ),
  selectizeInput('gpus', 'GPUs on machine', choices = c(1:4), selected = 4, multiple = F),
  uiOutput('nucleic'),
  uiOutput('model_ui'),
  textInput(
    'model_custom', 'Or enter a specific dorado model name',
    value = '', placeholder = 'e.g. dna_r10.4.1_e8.2_400bps_hac@v4.2.0'
  ),
  selectizeInput('readformat', 'Output format', choices = c('fastq', 'bam'), selected = 'fastq'),
  uiOutput('mods'),
  checkboxInput('adaptive', 'Adaptive sampling run', value = F),
  uiOutput('as_file'), #render conditionally if adaptive sampling
  shinyDirButton("pod5", "Select pod5 folder", title ='Please select a folder with signal data', multiple = F),
  checkboxInput('recursive', 'Search pod5 recursively'),
  
  checkboxInput('barcoded', 'Barcoded run'),
  # uiOutput('minknow_output'),
  uiOutput('kits'),
  #tags$hr(),
  textInput('session_name', 'Name for new session (optional)', value = 'tgs'),
)

ui <- page_navbar(
  tags$head(
    tags$style(
      HTML("
      :root {
        --brand-primary: #2C3E50;
        --surface: #ffffff;
        --surface-muted: #f4f6f8;
        --border: #e3e7eb;
      }

      body {
        background-color: var(--surface-muted);
      }

      .navbar {
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      }

      .navbar-controls {
        font-weight: 600;
        letter-spacing: 0.2px;
      }

      /* toolbar of action buttons, aligned to the top of the sidebar */
      .sidebar-toolbar {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 0.5rem;
        padding-bottom: 1rem;
        margin-bottom: 1rem;
        border-bottom: 1px solid var(--border);
      }

      .sidebar-toolbar .btn {
        width: 100%;
        white-space: normal;
        line-height: 1.2;
        border-radius: 8px;
        border: none;
        font-size: 0.78rem;
        padding: 0.5rem 0.4rem;
        box-shadow: 0 1px 2px rgba(0,0,0,0.08);
        transition: transform 0.15s ease, box-shadow 0.15s ease;
      }

      .sidebar-toolbar .btn:hover {
        transform: translateY(-1px);
        box-shadow: 0 3px 8px rgba(0,0,0,0.12);
      }

      .hover-action-button {
        background-color: #e3f2fb;
        color: #14425e;
      }

      .bslib-sidebar-layout > .sidebar {
        background-color: var(--surface);
        border-right: 1px solid var(--border);
      }

      .card {
        border: 1px solid var(--border);
        border-radius: 12px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.05);
      }

      .form-control, .selectize-input {
        border-radius: 8px;
      }

      .progress {
        transform: rotate(180deg);
        background: linear-gradient(to left, #eaecee, #ff4d4d);
        border-radius: 6px;
        height: 10px;
      }
      .progress-bar {
        background: #eaecee;
        border-radius: 6px;
      }
      ")
    )
  ),

  useShinyjs(),
  use_hover(),
  fillable = T,
  title = tags$div(
    class = 'navbar-controls',
    style = 'display:flex; align-items:center; gap:0.5rem;',
    bsicons::bs_icon('cpu'),
    tags$span('ONT basecaller app')
  ),
  theme = bs_theme(
    version = 5,
    font_scale = 0.9,
    bootswatch = 'yeti',
    primary = '#2C3E50',
    base_font = font_google('Inter'),
    heading_font = font_google('Inter'),
    "border-radius" = "0.6rem"
  ),
  sidebar = sidebar,
  nav_panel(
    title = "",
    uiOutput('progressbars'),
    #verbatimTextOutput('pod5_selected'),
    card(max_height = '250px',
    reactableOutput('tmux_table')
    ),
    card(max_height = '400px',
    verbatimTextOutput('stdout')
    )
  )
)

### secure app -----------------------------###
ui <- secure_app(ui,theme = "simplex")
credentials <- readRDS("credentials.rds")

server <- function(input, output, session) {

  # start disabled until a valid pod5 folder is selected
  shinyjs::disable('start')
  
  # the model actually in effect: a typed custom model name takes
  # precedence over the fast/hac/sup preset when non-empty
  effective_model <- reactive({
    custom <- input$model_custom
    if (is.null(custom)) custom <- ''
    custom <- trimws(custom)
    if (nzchar(custom)) custom else input$model
  })

  # TRUE when a specific/custom model name has been typed in
  using_custom_model <- reactive({
    custom <- input$model_custom
    !is.null(custom) && nzchar(trimws(custom))
  })

  # the nucleic acid actually in effect: while a custom model is typed in,
  # the picker is hidden, so infer dna/rna from the model name itself
  # (dorado model names start with 'dna_' or 'rna_')
  effective_nucleic <- reactive({
    if (using_custom_model()) {
      if (grepl('^rna', tolower(trimws(effective_model())))) 'rna' else 'dna'
    } else if (is.null(input$nucleic)) {
      'dna'
    } else {
      input$nucleic
    }
  })

  # build the dorado command-line args for a given pod5 dir; shared by the
  # live preview and the actual 'start' handler so they never drift apart
  build_cmd_args <- function(pod5dir, as_file_path = NA) {
    mods_vec <- effective_model()
    # only combine the model with selected mod tags for the short
    # basecalling-speed names (hac/sup, i.e. not 'fast') and BAM output -
    # never for a specific/custom model name
    if (!using_custom_model() && !is.null(input$readformat) && input$readformat == 'bam' && !is.null(input$mod) && model_base(effective_model()) != 'fast') {
      mods_vec <- c(mods_vec, input$mod)
    }
    mods_vec <- unlist(mods_vec)
    mods_vec <- mods_vec[!is.na(mods_vec) & mods_vec != '' & mods_vec != 'none']
    model_arg <- paste(mods_vec, collapse = ',')
    if (model_arg == '') model_arg <- effective_model()
    cmd_args <- c(dorado_script(), '-p', pod5dir, '-m', model_arg)
    if (isTRUE(input$recursive)) cmd_args <- c(cmd_args, '-r')
    if (isTRUE(input$barcoded)) cmd_args <- c(cmd_args, paste0('-k', input$kit))
    if (isTRUE(input$adaptive) && !is.null(as_file_path) && !is.na(as_file_path) && nzchar(as_file_path)) {
      cmd_args <- c(cmd_args, paste0('-l', as_file_path))
    }
    if (!is.null(input$readformat) && input$readformat == 'bam') cmd_args <- c(cmd_args, '-b')
    cmd_args
  }

  # live command preview, shown once a pod5 folder and a model are selected
  cmd_preview <- reactive({
    req(input$pod5)
    if (is.integer(input$pod5)) return(NULL)
    pod5dir <- parseDirPath(volumes, input$pod5)
    if (length(pod5dir) == 0 || !nzchar(pod5dir)) return(NULL)
    model <- effective_model()
    if (is.null(model) || !nzchar(model)) return(NULL)

    as_file_path <- NA
    if (isTRUE(input$adaptive) && !is.null(input$decision_file)) {
      as_file <- parseFilePaths(volumes, input$decision_file)
      if (nrow(as_file) > 0) as_file_path <- as_file$datapath
    }
    paste(build_cmd_args(pod5dir, as_file_path), collapse = ' ')
  })

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

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
  
  shinyFileChoose(
    input, 'decision_file',
    roots = volumes, filetypes = 'csv',
    session = session
  )
  
  
  # track tmux sessions
  # empty df for init
  empty_df <- data.frame(
    session_id = NA,
    user = NA,
    started = NA,
    runtime = NA,
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
        user = str_split_i(tmuxinfo, " ", 7),
        started = str_split_i(tmuxinfo, " ", 1) %>% as.numeric() %>% as.POSIXct(),
        runtime = NA,
        active = str_split_i(tmuxinfo, " ", 6),
        attached = str_split_i(tmuxinfo, " ", 3),
        session_path = str_split_i(tmuxinfo, " ", 4)
      ) %>%
       mutate(
         runtime = difftime(now(), started, units = 'auto'),
         attached = if_else(as.numeric(attached) == 1, 'yes', 'no'),
         active = if_else(as.numeric(active) == 1, 'yes', 'no')
      ) %>%
       mutate(runtime = paste0(round(runtime ,1), " ", units(runtime))
      ) %>%
       arrange(started)
    }
  })
  
  selected <- reactive({
    getReactableState('tmux_table', 'selected')
  })
  
  
  # progress bars
  if (is_bin_on_path('nvidia-smi')) {
    cmd <- 'nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits'
  } else {
    cmd <- 'tail -n 4 data/smi.txt'
  }
  
  newLines <- reactive({
    req(input$gpus)
    invalidateLater(1000, session)
    readLines(pipe(cmd)) %>% as.numeric() %>% tail(as.numeric(input$gpus))
  })
  
  # make progress bars
  output$progressbars <- renderUI({
    req(input$gpus)
    values <- rep(10, input$gpus) #newLines()
    fluidRow(
     lapply(1:input$gpus, function(x) {
       column(
         width = 12/as.numeric(input$gpus), 
         progressBar(id = paste0('pb', x), value = 100 - values[x], display_pct = F, title = paste0('GPU', x)))
     })
    )
  })
  
  observe({
    req(input$gpus)
    gpuvalues <- newLines()
    lapply(1:input$gpus, function(x) {
      updateProgressBar(
        session = session,
        id = paste0('pb', x),
        value = 100 - gpuvalues[x], title = paste0('GPU', x)
      )
    })
  })
  
  # start basecalling
  observeEvent(input$start, {
    
    new_session_name <- paste0(digest::digest(runif(1), algo = 'crc32'), '-', input$session_name)
    pod5dir <- parseDirPath(volumes, input$pod5)
    as_file <- parseFilePaths(volumes, input$decision_file)
    # launch new session
    
    args1 <- c('new', '-d', '-s', new_session_name)
    system2('tmux', args = args1)
    
    as_file_path <- if (isTRUE(input$adaptive) && nrow(as_file) > 0) as_file$datapath else NA
    cmd_args <- build_cmd_args(pod5dir, as_file_path)
    
    # execute dorado in the new session
    string <- paste(cmd_args, collapse = ' ')
    args2 <- c('send-keys', '-t', new_session_name, shQuote(string), 'C-m')
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

  #nvidia-smi
  observeEvent(input$show_gpu, {
    shinyjs::html(id = "stdout", html = "")

    nvidia_smi_p <- tryCatch(
      processx::run(
        'nvidia-smi',
        #pty = TRUE,
        error_on_status = FALSE
      ),
      error = function(e) NULL
    )

    shinyjs::html(id = "stdout", html = paste0('<pre>', htmltools::htmlEscape(nvidia_smi_p$stdout), '</pre>'), add = T);
    runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
    
  })
  
  # run a dorado subcommand and get back its combined stdout+stderr as if
  # it were attached to a real terminal (dorado's logger changes behaviour
  # when writing to a plain pipe). Tries processx's built-in pty support
  # first; if that fails (e.g. no controlling terminal is available to the
  # R/Shiny process in some hosting setups), falls back to wrapping the
  # call in the 'script' utility, which allocates its own pty. Returns
  # list(ok = TRUE, output = <text>) or list(ok = FALSE, error = <msg>).
  run_dorado_pty <- function(args) {
    res <- tryCatch(
      processx::run('dorado', args = args, pty = TRUE, error_on_status = FALSE),
      error = function(e) e
    )
    if (!inherits(res, 'error') && !is.null(res$stdout)) {
      return(list(ok = TRUE, output = res$stdout))
    }
    pty_error <- if (inherits(res, 'error')) conditionMessage(res) else 'pty run returned no stdout'

    script_bin <- Sys.which('script')
    if (nzchar(script_bin)) {
      is_mac <- Sys.info()[['sysname']] == 'Darwin'
      script_args <- if (is_mac) {
        # BSD script (macOS): script [-q] file command...
        c('-q', '/dev/null', 'dorado', args)
      } else {
        # util-linux script (Linux): script -qec "command" file
        c('-qec', paste(shQuote(c('dorado', args)), collapse = ' '), '/dev/null')
      }
      res2 <- tryCatch(
        processx::run('script', args = script_args, error_on_status = FALSE),
        error = function(e) e
      )
      if (!inherits(res2, 'error') && !is.null(res2$stdout)) {
        return(list(ok = TRUE, output = res2$stdout))
      }
      script_error <- if (inherits(res2, 'error')) conditionMessage(res2) else 'script fallback returned no stdout'
      return(list(ok = FALSE, error = paste0('pty: ', pty_error, ' | script fallback: ', script_error)))
    }

    list(ok = FALSE, error = pty_error)
  }

  # show available dorado models
  # 'dorado download --list' writes its listing to stderr as log lines
  # (e.g. "[...] [info]  - dna_r10.4.1_e8.2_400bps_hac@v4.2.0"), grouped
  # under "> section name" headers. We keep only the entries under the
  # "simplex models" and "modification models" sections, print the
  # filtered list to stdout, and show it in the app's output panel.
  observeEvent(input$show_models, {
    shinyjs::html(id = "stdout", html = "")

    version_res <- run_dorado_pty(c('--version'))
    version_text <- if (version_res$ok) {
      v <- gsub('\033\\[[0-9;]*[a-zA-Z]', '', version_res$output)
      v <- trimws(gsub('\r\n|\n|\r', ' ', v))
      if (nzchar(v)) v else 'unknown'
    } else {
      'unknown'
    }

    p <- run_dorado_pty(c('download', '--list'))

    if (!p$ok) {
      notify_failure(paste0('Could not run "dorado download --list": ', p$error), timeout = 5000, position = 'center-bottom')
      shinyjs::html(id = "stdout", html = paste0('<pre>', htmltools::htmlEscape(paste0('Error running dorado: ', p$error)), '</pre>'), add = T)
      runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
      return()
    }

    # strip ANSI colour codes and normalise CRLF/CR line endings
    plain_text <- gsub('\033\\[[0-9;]*[a-zA-Z]', '', p$output)
    raw_lines <- strsplit(plain_text, '\r\n|\n|\r')[[1]]
    # strip the leading "[timestamp] [level] " log prefix
    clean_lines <- sub('^\\[[^]]*\\]\\s*\\[[^]]*\\]\\s*', '', raw_lines)

    wanted_sections <- c('simplex models', 'modification models')
    section <- NA_character_
    keep <- character(0)
    for (line in clean_lines) {
      if (grepl('^>\\s*', line)) {
        section <- trimws(sub('^>\\s*', '', line))
        next
      }
      if (!is.na(section) && section %in% wanted_sections && grepl('^\\s*-\\s*', line)) {
        keep <- c(keep, trimws(sub('^\\s*-\\s*', '', line)))
      }
    }

    if (length(keep) == 0) {
      models_text <- 'No simplex or modification models found in "dorado download --list" output.'
    } else {
      models_text <- paste(keep, collapse = '\n')
    }
    out_text <- paste0('dorado version: ', version_text, '\n\n', models_text)

    # print the filtered list to stdout (the original listing was on stderr)
    cat(out_text, sep = '\n')

    shinyjs::html(id = "stdout", html = paste0('<pre>', htmltools::htmlEscape(out_text), '</pre>'), add= T);
    runjs("document.getElementById('stdout').parentElement.scrollTo({ top: 1e9, behavior: 'smooth' });")
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
    req(input$barcoded)
    if (input$barcoded) {
      updateCheckboxInput('recursive', value = T, session = session)
    } else {
      updateCheckboxInput('recursive', value = F, session = session)
    }
  })
  
  dorado_script <- reactiveVal()
  dorado_script('ont-basecall.sh')
  
  # outputs
  # show kits if barcoded run
  output$kits <- renderUI({
    req(input$barcoded)
    if (input$barcoded) {
      selectizeInput('kit', 'Barcoding kit', choices = barcoding_kits, selected = 'SQK-RBK114-96')
    } else {
      NULL
    }
  })
  
  output$as_file <- renderUI({
    req(input$adaptive)
    if (input$adaptive) {
      #checkboxInput('folder_output', 'Output in folders', value = TRUE)
      shinyFilesButton(
        'decision_file', 
        'AS decisions file', 
        title = "Select adaptive sampling decisions file (AS_decisions.csv)", 
        multiple = F)
    }
  })

  # basecalling speed preset (fast/hac/sup) - hidden once a specific
  # model name is typed in, since it no longer applies
  output$model_ui <- renderUI({
    if (using_custom_model()) return(NULL)
    current <- isolate(input$model)
    selectizeInput(
      "model", "Short model name",
      choices = c('fast', 'hac', 'sup'),
      selected = if (is.null(current)) 'fast' else current
    )
  })

  # nucleic acid selection (dna | rna) - hidden once a specific
  # model name is typed in, since it's implied by that model name
  output$nucleic <- renderUI({
    req(input$readformat)
    if (using_custom_model()) return(NULL)
    req(effective_model())
    current <- isolate(input$nucleic)
    selectInput('nucleic', 'Nucleic acid', choices = c('dna', 'rna'),
                selected = if (is.null(current)) 'dna' else current)
  })

  # show modification models only when BAM output is selected and model != 'fast'
  output$mods <- renderUI({
    req(input$readformat, effective_model())
    base <- model_base(effective_model())
    if (input$readformat == 'bam' && !is.na(base) && base != 'fast') {
      # define choices based on nucleic and model
      dna_choices <- c('none', '4mC_5mC', '5mCG_5hmCG', '5mC_5hmC', '6mA')
      rna_hac_choices <- c('none', 'm5C', 'm6A_DRACH', 'inosine_m6A', 'pseU')
      rna_sup_choices <- c('none', 'm5C_2OmeC', 'm6A_DRACH', 'inosine_m6A_2OmeA', 'pseU_2OmeU', '2OmeG')

      choices <- switch(effective_nucleic(),
                        'dna' = dna_choices,
                        'rna' = if (base == 'hac') rna_hac_choices else if (base == 'sup') rna_sup_choices else c('none'))

      selectizeInput('mod', 'Modification model', choices = choices, selected = 'none', multiple = TRUE)
    } else {
      NULL
    }
  })

  # ensure mod choices/selection stay valid when model or nucleic changes
  observe({
    req(input$readformat, effective_model())
    base <- model_base(effective_model())
    if (!(input$readformat == 'bam' && !is.na(base) && base != 'fast')) {
      return()
    }
    # compute allowed choices
    dna_choices <- c('none', '4mC_5mC', '5mCG_5hmCG', '5mC_5hmC', '6mA')
    rna_hac_choices <- c('none', 'm5C', 'm6A_DRACH', 'inosine_m6A', 'pseU')
    rna_sup_choices <- c('none', 'm5C_2OmeC', 'm6A_DRACH', 'inosine_m6A_2OmeA', 'pseU_2OmeU', '2OmeG')
    choices <- switch(effective_nucleic(),
      'dna' = dna_choices,
      'rna' = if (base == 'hac') rna_hac_choices else if (base == 'sup') rna_sup_choices else dna_choices
    )

    sel <- isolate(input$mod)
    # drop selections not in choices
    if (!is.null(sel)) {
      valid_sel <- sel[sel %in% choices]
      if (length(valid_sel) == 0) valid_sel <- 'none'
      # if 'none' is among multiple selections, remove it
      if (length(valid_sel) > 1 && 'none' %in% valid_sel) valid_sel <- valid_sel[valid_sel != 'none']
      updateSelectizeInput(session, 'mod', choices = choices, selected = valid_sel)
    } else {
      updateSelectizeInput(session, 'mod', choices = choices, selected = 'none')
    }
  })

  # auto-remove 'none' if any other modification is selected
  observeEvent(input$mod, {
    sel <- input$mod
    if (is.null(sel)) return()
    # when multiple mods selected and 'none' is among them, drop 'none'
    if (length(sel) > 1 && 'none' %in% sel) {
      newsel <- sel[sel != 'none']
      updateSelectizeInput(session, 'mod', selected = newsel)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  output$tmux_table <- renderReactable({
    reactable(
      empty_df,
      #tmux_sessions(), 
      pagination = FALSE, highlight = TRUE, height = 200, compact = T, 
      fullWidth = T, selection = 'single', onClick = 'select', defaultSelected = 1,
      theme = reactableTheme(
        rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ff0000")
      ),
      columns = list(
        started = colDef(minWidth = 130, format = colFormat(datetime = T, locales = 'swe-SE')),
        runtime = colDef(minWidth = 70), #format = colFormat(suffix = ' h', digits = 2)),
        #command = colDef(minWidth = 50),
        active = colDef(minWidth = 70, show = F),
        attached = colDef(minWidth = 70, show = F),
        session_path = colDef(minWidth = 250)
      )
    )
  })
  
  observe({
    updateReactable('tmux_table', data = tmux_sessions(), selected = selected())
  })
  
  # handle pod5 directory
  observe({
    req(input$pod5)
    if (is.integer(input$pod5)) {
      shinyjs::disable('start')
    } else {
      pod5dir <- parseDirPath(volumes, input$pod5)
      pod5files <- length(list.files(pod5dir, pattern = '*.pod5', recursive = input$recursive))
      if (pod5files > 0) {
        shinyjs::enable('start')
      } else {
        shinyjs::disable('start')
      }
    }
  })
  
  output$stdout <- renderText({
    req(input$pod5)
    if (is.integer(input$pod5)) {
      "No directory has been selected"
    } else {
      pod5dir <- parseDirPath(volumes, input$pod5)
      pod5files <- length(list.files(pod5dir, pattern = '*.pod5', recursive = input$recursive))
      cmd <- cmd_preview()
      cmd_line <- if (is.null(cmd)) '' else paste0('\n\nCommand: ', cmd)
      paste0(
        'Selected pod5 directory: ', pod5dir, '\n',
        pod5files, ' pod5 files found', cmd_line)
    }
  })
  
  output$pod5_selected <- renderText({
    req(input$pod5)
    if (is.integer(input$pod5)) {
      "No directory has been selected"
    } else {
      pod5dir <- parseDirPath(volumes, input$pod5)
      pod5dir
    }
  })

}

shinyApp(ui, server)
