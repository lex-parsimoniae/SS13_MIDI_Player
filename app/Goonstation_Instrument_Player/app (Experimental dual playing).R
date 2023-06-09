#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# libraries ----
suppressPackageStartupMessages({
  library(shiny)
  #library(here)
  library(shinyjs)
  library(shinyBS)
  library(shinydashboard)
  #library(qpcR)
  library(tidyverse)
  #library(readr)
  #library(png)
  #library(ggpubr)
  #library(grid)
  library(rclipboard)
  #library(DT)
  #library(DescTools)
  #library(webexercises)
  library(zoo)
  library(tuneR)
  library(DT)
})

# user interface ----
shinyjs::useShinyjs()

## Tabs ----
### Intro ----
intro_tab <- tabItem(
  tabName = "intro_tab",
  box(width = 12,
      collapsible = TRUE,
      tags$h2("What is this?"),
      tags$img(src="img/instruments.png", width="315", style="display: block; margin-left: auto; margin-right: auto;"),
      tags$br(),
      #imageOutput("instruments_pic"),
      tags$p("This R Shiny web application takes as input uploaded MIDI files and outputs Python scripts that simulate keypresses on virtual instruments. This app was designed with the Goonstation branch of SS13 in mind, 
             but should theoretically be compatible with any virtual instrument that allows one to customize keypresses. To run the generated Python scripts, you will need a local installation of ", a(href = 'https://www.python.org/downloads/', 'Python', .noWS = "outside"), " and the ", a(href = 'https://pypi.org/project/pynput/', 'pynput', .noWS = "outside"),  " module.
             With a local installation of Python, these can be installed by opening the terminal and executing the following command:"),
      tags$br(),
      code("pip install pynput"),
      tags$br(),
      tags$br(),
      tags$h2("Keybinds"),
      tags$p("Because pynput doesn't support some of the keys used in the default keybindings, you will need to use custom keybinds. They are:"),
      dataTableOutput("keybinds_data"),
      tags$br(),
      tags$h2("Pausing"),
      tags$p("These scripts don't have inbuilt functionality for pausing, but I use ", a(href = 'https://www.autohotkey.com/v2/', 'AutoHotKey', .noWS = "outside"), " to bind .cmd/.bat files that suspend the Python process (sourced from ", a(href = 'https://github.com/craftwar/suspend', 'here', .noWS = "outside"), ") to keybinds (f8 to pause, f9 to resume). To use this functionality, simply install AutoHotKey (v2), download and extract the files in the ", a(href = 'https://github.com/E-Y-M/Goonstation_Instrument_Player/tree/main/Script%20pausing', 'Script pausing', .noWS = "outside"),  " folder, and open 'Keyboard pause - resume.ahk' in the background. Other than that, once you have the script, simply paste the keybinds into the instrument, open the script (it has a 5s buffer time), and tab into the instrument.")
  )
)

### Convert ----
convert_tab <- tabItem(
  tabName = "convert_tab",
  box(width = 12,
      collapsible = FALSE,
      title = "File upload and parameter specifications",
      fileInput(
        "midi_upload",
        "Upload your MIDI",
        multiple = FALSE,
        accept = c(".mid", ".midi")),
      radioButtons(
        "multi_instrument",
        "Multiple instruments?",
        choices = c("Yes", "No"),
        choiceNames = c("Yes", "No"),
        selected = "No"
      ),
      numericInput(
        "instrument1_id",
        "Instrument 1 ID",
        999999
      ),
      numericInput(
        "instrument2_id",
        "Instrument_2_ID",
        999999
      ),
      radioButtons(
        "instrument",
        "Instrument to generate script for. Note that some instruments have restricted ranges. For these instruments, notes outside the range will be 'compressed' to the instrument's available range (e.g., for an instrument with a lower bound of C3, C1 and C2 will be converted to C3)",
        choices = c("Piano (C2 - C7)",
                    "Banjo (E3 - C6)",
                    "Trumpet (E3 - C6)",
                    "Saxophone (G3 - C6)",
                    "Fiddle (A3 - G6)"),
        selected = "Piano (C2 - C7)",
        choiceNames = c("Piano (C2 - C7)",
                        "Banjo (E3 - C6)",
                        "Trumpet (E3 - C6)",
                        "Saxophone (G3 - C6)",
                        "Fiddle (A3 - G6)")
      ),
      numericInput("tempo_adjust",
                   "Tempo modifier",
                   value = 1,
                   min = .01),
      bsTooltip("tempo_adjust",
                "Multiplier to apply to the default tempo (e.g., 1.5 = 150% faster)",
                placement = "left",
                trigger = "hover"),
      dataTableOutput("track_data"),
      textInput("instrument1_tracks",
                "Instrument 1 tracks",
                ""),
      textInput("instrument2_tracks",
                "Instrument 2 tracks",
                ""),
      actionButton(
        "convert_start",
        "Convert MIDI",
        width = '100%',
        class = "btn-success"),
      downloadButton("download_script", "Download")
  )
)


# Define UI for application that draws a histogram
## UI ----
skin_color <- "black"

ui <- dashboardPage(
  skin = skin_color,
  dashboardHeader(title = "Goonstation Instrument Player", 
                  titleWidth = "calc(100% - 44px)" # puts sidebar toggle on right
  ),
  dashboardSidebar(
    # https://fontawesome.com/icons?d=gallery&m=free
    sidebarMenu(
      id = "tabs",
      menuItem("How this app works", tabName = "intro_tab", icon = icon("cog")),
      menuItem("Convert MIDI", tabName = "convert_tab", icon = icon("music"))
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      # links to files in www/
      tags$link(rel = "stylesheet", type = "text/css", href = "basic_template.css"), 
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), 
      tags$script(src = "custom.js")
    ),
    tabItems(
      intro_tab,
      convert_tab
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  shinyjs::disable("download_script")
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  midi = reactiveValues(midi_input = NULL,
                        midi_output = NULL,
                        songTitle = NULL)
  
  output$instruments_pic = renderImage({
    list(src = "www/img/instruments.png",
         width = "315px",
         height = "303px",
         alt = "Instruments")
  }, deleteFile = FALSE)
  
  # Set the keybinds ----
  ## Piano 
  keybinds_piano_low = data.frame(key = c("Q", "W", "E", "R", "T", "Y", "U", "I", 
                                          "O", "P", "A", "S", "D", "F", "G", "H",
                                          "J", "K", "L", "Z", "X", "C", "V", "B"),
                                  new_note = c("C2", "C#2", "D2", "D#2", "E2", "F2", "F#2", "G2", "G#2", "A2", "A#2", "B2",
                                               "C3", "C#3", "D3", "D#3", "E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3"))
  
  keybinds_piano_high = data.frame(key = c("q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                           "a", "s", "d", "f", "g", "h", "j", "k", "l",
                                           ";", "z", "x", "c", "v", "b", "n", "m", "1",
                                           "2", "3", "4", "5", "6", "7", "8", "9", "0"),
                                   new_note = c("C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                                "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                                                "C6", "C#6", "D6", "D#6", "E6", "F6", "F#6", "G6", "G#6", "A6", "A#6", "B6", "C7"))
  
  keybinds_piano_universal = data.frame(key = c("1","!","2","@","3","4","$","5","%","6","^","7","8","*","9","(","0","q","Q","w","W","e","E",
                                                "r","t","T","y","Y","u","i","I","o","O","p","P","a","s","S","d","D","f","g","G","h","H","j",
                                                "J","k","l","L","z","Z","x","c","C","v","V","b","B","n","m"),
                                        new_note = c("C2", "C#2", "D2", "D#2", "E2", "F2", "F#2", "G2", "G#2", "A2", "A#2", "B2",
                                                     "C3", "C#3", "D3", "D#3", "E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3",
                                                     "C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                                     "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                                                     "C6", "C#6", "D6", "D#6", "E6", "F6", "F#6", "G6", "G#6", "A6", "A#6", "B6", "C7"))
  
  keybinds_piano = rbind(keybinds_piano_low,
                         keybinds_piano_high)
  
  
  keybinds_piano_paste = keybinds_piano %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
  ## Sax 
  keybinds_sax = data.frame(key = c("Z", "X", "C", "V", "B", "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                    "a", "s", "d", "f", "g", "h", "j", "k", "l",
                                    ";", "z", "x", "c", "v", "b"),
                            new_note = c("G3", "G#3", "A3", "A#3", "B3",
                                         "C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                         "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                                         "C6"))
  
  keybinds_sax_paste = keybinds_sax %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
  ## Violin 
  keybinds_violin = data.frame(key = c("C", "V", "B", "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                       "a", "s", "d", "f", "g", "h", "j", "k", "l",
                                       ";", "z", "x", "c", "v", "b", "n", "m", "1",
                                       "2", "3", "4", "5"),
                               new_note = c("A3", "A#3", "B3",
                                            "C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                            "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                                            "C6", "C#6", "D6", "D#6", "E6", "F6", "F#6", "G6"))
  
  keybinds_violin_paste = keybinds_violin %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
  ## Banjo (Transpose of -8 from Piano)
  keybinds_banjo = data.frame(key = c("J", "K", "L", "Z", "X", "C", "V", "B", "q", "w", "e",
                                      "r", "t", "y", "u", "i", "o", "p", "a", "s", "d", "f",
                                      "g", "h", "j", "k", "l", ";", "z", "x", "c", "v", "b"),
                              new_note = c("E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3",
                                           "C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                           "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5", "C6"))
  
  keybinds_banjo_paste = keybinds_banjo %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
  # Dataframe for easy keybind copying ----
  datafiles = reactiveValues(
    keybinds_data = data.frame()
  )
  
  datafiles$keybinds_data = data.frame(
    Instrument = c("Piano",
                   "Banjo",
                   "Trumpet",
                   "Sax",
                   "Fiddle"),
    Keybinds = c(keybinds_piano_paste,
                 keybinds_banjo_paste,
                 keybinds_banjo_paste,
                 keybinds_sax_paste,
                 keybinds_violin_paste),
    Copy = shinyInput(actionButton, 5,
                         'button_',
                         label = "Copy",
                         onclick = paste0('Shiny.onInputChange( \"select_button\" , this.id)')))
  
  output$keybinds_data = renderDataTable({
    datafiles$keybinds_data
  }, escape = FALSE, rownames = FALSE, options = list(dom = 't'))
  
  ## Function to copy the keybinds for a selected row ----
  observeEvent(input$select_button, {
    selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
    writeClipboard(str_trim(as.character(datafiles$keybinds_data[selectedRow,2])))
  })
  
  # Load in midi to get the tracks ----
  observeEvent(input$midi_upload, {
    midi_tracks = readMidi(input$midi_upload$datapath) %>% 
      arrange(track, parameterMetaSystem) %>% 
      select(track, parameterMetaSystem, event) %>% 
      distinct() %>% 
      filter(event == "Sequence/Track Name") %>% 
      rename("Instrument" = parameterMetaSystem,
             "Track" = track) %>% 
      select(-event)
    
    all_tracks_paste = paste0(midi_tracks$Track,
                              collapse = ",")
    
    updateTextInput(session,
                    "instrument1_tracks",
                    value = all_tracks_paste)
    
    updateTextInput(session,
                    "instrument2_tracks",
                    value = all_tracks_paste)
    
    output$track_data = renderDataTable({
      midi_tracks
    }, escape = FALSE, rownames = FALSE, options = list(dom = 't'))
  })
  
  # Main conversion script ----
  observeEvent(input$convert_start, {
    shinyjs::disable("download_script")
    req(input$midi_upload)
    
    showModal(modalDialog(
      title = "Conversion in progress",
      "Converting, please wait"
    ))
    
    ## Set instrument and keybinds ----
    if (input$instrument == "Piano (C2 - C7)") {
      keybinds = keybinds_piano
      midi$instrumentName = "Piano"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 2, 2,
                               ifelse(octave > 6 & note != "C", 6, octave)),
               new_note = paste0(note, octave)) %>% 
        mutate(new_note = ifelse(new_note == "C8", "C7", new_note)) %>% 
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    } else if (input$instrument == "Saxophone (G3 - C6)") {
      keybinds = keybinds_sax
      midi$instrumentName = "Saxophone"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 3, 3,
                               ifelse(octave > 5 & note != "C", 5, octave))) %>% 
        mutate(octave = ifelse(octave > 5 & note == "C", 6, octave),
               new_note = paste0(note, octave)) %>% 
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    } else if (input$instrument == "Fiddle (A3 - G6)") {
      keybinds = keybinds_violin
      midi$instrumentName = "Fiddle"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 3, 3,
                               ifelse(octave > 6, 6, octave))) %>% 
        mutate(new_note = paste0(note, octave)) %>% 
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    } else if (input$instrument == "Banjo (E3 - C6)") {
      keybinds = keybinds_banjo
      midi$instrumentName = "Banjo - Trumpet"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 3, 3,
                               ifelse(octave > 5 & note != "C", 5, octave))) %>% 
        mutate(octave = ifelse(octave > 5 & note == "C", 6, octave),
               new_note = paste0(note, octave)) %>%
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    } else { # Trumpet
      keybinds = keybinds_banjo
      midi$instrumentName = "Banjo - Trumpet"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 3, 3,
                               ifelse(octave > 5 & note != "C", 5, octave))) %>% 
        mutate(octave = ifelse(octave > 5 & note == "C", 6, octave),
               new_note = paste0(note, octave)) %>%
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    }
    
    songTitle = gsub("_", " ", input$midi_upload)
    songTitle = gsub(".mid", "", songTitle)
    songTitle = paste0(songTitle, " (", midi$instrumentName, ")")
    midi$songTitle = songTitle
    
    keybinds_paste = keybinds %>% 
      select(key) %>% 
      as.list() %>% 
      unlist() %>% 
      paste0(collapse = "")
    
    message("Set MIDI key and keybinds")
    
    ## Read in the midi ----
    input_midi = readMidi(input$midi_upload$datapath)
    
    message("Read in MIDI #1")
    
    #t_midi_tpb = mido$MidiFile(input$midi_upload$datapath)
    track_division_1 = parse_number(unlist(strsplit(input$instrument1_tracks, split = ",")))
    track_division_2 = parse_number(unlist(strsplit(input$instrument2_tracks, split = ",")))
    
    ticks_per_beat = 480
    
    if (input$multi_instrument == "Yes") {
      midi_processed = input_midi %>% 
        select(time, event, parameter1, track) %>% 
        rename("type" = event,
               "number" = parameter1) %>% 
        filter(grepl("Note On", type)) %>%
        mutate(number = as.numeric(number),
               instrument = ifelse(track %in% track_division_1, 1, 2)) %>% 
        left_join(midi_key) %>% 
        group_by(time, instrument) %>% 
        mutate(key = ifelse(is.na(key), "", key)) %>% 
        summarise(notes_vec = paste(key, collapse = "")) %>% 
        distinct() %>% 
        ungroup() %>% 
        mutate(delay_after = NA)
    } else {
      midi_processed = input_midi %>% 
        select(time, event, parameter1, track) %>% 
        rename("type" = event,
               "number" = parameter1) %>% 
        filter(grepl("Note On", type)) %>%
        mutate(number = as.numeric(number),
               instrument = 1) %>% 
        left_join(midi_key) %>% 
        group_by(time, instrument) %>% 
        mutate(key = ifelse(is.na(key), "", key)) %>% 
        summarise(notes_vec = paste(key, collapse = "")) %>% 
        distinct() %>% 
        ungroup() %>% 
        mutate(delay_after = NA)
    }
    
    time_division = input_midi %>% 
      filter(grepl("clocks/tick", parameterMetaSystem)) %>% 
      mutate(division = sub("...,", "", parameterMetaSystem))
    
    ### Get the tempo shifts ----
    tempos = input_midi %>% 
      select(time, event, parameterMetaSystem) %>% 
      `colnames<-` (c("time", "type", "tempo")) %>% 
      filter(grepl("Tempo", type)) %>% 
      select(time, tempo)
    
    midi_processed = midi_processed %>% 
      left_join(tempos)
    
    midi_processed$tempo[1] = tempos$tempo[1]
    
    midi_processed = midi_processed %>% 
      mutate(tempo = as.numeric(na.locf(tempo)))
    
    min_tempo = as.numeric(min(as.numeric(midi_processed$tempo)))
    
    message("Up to dealing with repeats")
    
    ### Remove repeats in the strings
    for (i in 1:nrow(midi_processed)) {
      curr_vec = midi_processed$notes_vec[i]
      
      vec_split = unlist(strsplit(curr_vec, split = ""))
      vec_unique = unique(vec_split)
      vec_paste = paste0(vec_unique, collapse = "")
      
      midi_processed$notes_vec[i] = vec_paste
    }
    
    message("Created note vectors")
    
    ## NEW: Pivot to wider for multiple instruments ----
    midi_processed = midi_processed %>% 
      pivot_wider(names_from = instrument,
                  values_from = notes_vec)
    
    message(midi_processed)
    
    `%nin%` = Negate(`%in%`)
    
    if ("2" %nin% colnames(midi_processed)) {
      midi_processed$`2` = NA
    }
    
    ### Deal with the close-in-time repeats ----
    for (i in 1:nrow(midi_processed)) {
      if (i == nrow(midi_processed)) {
        midi_processed$time[i] = midi_processed$time[i]
      } else if (abs(midi_processed$time[i] - midi_processed$time[i+1]) == 1) {
        midi_processed$time[i] = midi_processed$time[i+1]
      } else {
        midi_processed$time[i] = midi_processed$time[i]
      }
    }
    
    midi_processed = midi_processed %>%
      mutate(tempo = as.numeric(tempo),
             delay_after = NA)
    
    message("Dealt with close-in-time repeats")
    
    ### Add beat delays ----
    for (i in 1:nrow(midi_processed)) {
      if (midi_processed$time[i] < max(midi_processed$time)) {
        midi_processed$delay_after[i] = 
          midi_processed$time[i+1] - midi_processed$time[i]
      } else {
        midi_processed$delay_after[i] = 0
      }
    }
    
    message("Added beat delays")
    
    ### Set the time adjustment ----
    midi_write = midi_processed %>% 
      rowwise() %>% 
      mutate(min_tempo = as.numeric(min_tempo),
             tempo_adj = tempo/min_tempo,
             #real_time = (tempo / time) / 1e6,
             bpm = 60000000/tempo,
             bps = bpm / 60,
             tpb = ticks_per_beat,
             tps = tpb * bps,
             delay = (delay_after / tps)) %>% 
      ungroup() %>% 
      mutate(delay = delay * (1 / input$tempo_adjust),
             curr_instrument = case_when(is.na(`2`) & !is.na(`1`) ~ "1",
                                         !is.na(`2`) & is.na(`1`) ~ "2",
                                         !is.na(`2`) & !is.na(`1`) ~ "Both",
                                         is.na(`2`) & is.na(`1`) ~ "None"))
    
    message("Made any tempo adjustments")
    
    for (i in 1:nrow(midi_write)) {
      midi_write$action[i] = case_when(midi_write$curr_instrument[i] == "1" & midi_write$curr_instrument[i+1] == "2" ~ "switch_to_2",
                                       midi_write$curr_instrument[i] == "2" & midi_write$curr_instrument[i+1] == "1" ~ "switch_to_1",
                                       midi_write$curr_instrument[i] == "2" & midi_write$curr_instrument[i+1] == "Both" ~ "switch_to_1",
                                       midi_write$curr_instrument[i] == "Both" & midi_write$curr_instrument[i+1] == "1" ~ "switch_to_1",
                                       midi_write$curr_instrument[i] == "Both" & midi_write$curr_instrument[i+1] == "Both" ~ "switch_to_1",
                                       .default = "stay")
    }
    
    message("Assigned switching actions")
    
    # Figure out what the first focus (and action after) has to be
    if (input$multi_instrument == "No") {
      initial_action = "# PLACEHOLDER"
      initial_instrument = ""
    } else {
      if (midi_write$curr_instrument[1] == "1" | midi_write$curr_instrument[1] == "Both") {
        initial_action = paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ")")
        initial_instrument = " # Instrument 1"
      } else {
        initial_action = paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ")")
        initial_instrument = " # Instrument 2"
      }
    }
    
    message("Set initial window focus")
    
    ## Write to .py ----
    python_store = data.frame(`#command` = c(paste0("# Keybinds = ", keybinds_paste),
                                             "import time",
                                             "import win32gui",
                                             "import win32con",
                                             "import win32api",
                                             "from pynput.keyboard import Controller",
                                             #"from pynput.mouse import Button",
                                             #"from pynput.mouse import Controller as MouseController",
                                             "keyboard = Controller()",
                                             #"mouse = MouseController()",
                                             "time.sleep(5)",
                                             paste0(initial_action, initial_instrument)))
    
    ### NEW method for window switching (switch only when needed) ----
    if (input$multi_instrument == "Yes") {
      for (i in 1:nrow(midi_write)) {
        if (midi_write$curr_instrument[i] == "1") { # When the current instrument is 1
          if (midi_write$action[i] == "switch_to_2") { # When switching to instrument 2 afterwards
            curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$`1`[i], "')"),
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ") # Instrument 2"),
                                                  paste0("time.sleep(", midi_write$delay[i], ")"),
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ") # Instrument 2")))
          } else { # ...or if staying on instrument 1
            curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$`1`[i], "')"),
                                                  paste0("time.sleep(", midi_write$delay[i], ")")))
          }
        } else  if (midi_write$curr_instrument[i] == "2") { # When the current instrument is 2
          if (midi_write$action[i] == "switch_to_1") { # When switching to instrument 1 afterwards
            curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$`2`[i], "')"),
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ") # Instrument 1"),
                                                  paste0("time.sleep(", midi_write$delay[i], ")"),
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ") # Instrument 1")))
          } else { # ...or if staying on instrument 2
            curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$`2`[i], "')"),
                                                  paste0("time.sleep(", midi_write$delay[i], ")")))
          }
        } else { # When the current instrument is both
          if (midi_write$action[i] == "switch_to_1") { # When switching to instrument 1 afterwards
            curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$`1`[i], "')"),
                                                  "time.sleep(.015)",
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ") # Instrument 2"),
                                                  paste0("keyboard.type('", midi_write$`2`[i], "')"),
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ") # Instrument 1"),
                                                  paste0("time.sleep(", midi_write$delay[i], ")"),
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ") # Instrument 1")))
          } else { # ...or if staying on instrument 2
            curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$`1`[i], "')"),
                                                  "time.sleep(.015)",
                                                  paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ") # Instrument 2"),
                                                  paste0("keyboard.type('", midi_write$`2`[i], "')"),
                                                  paste0("time.sleep(", midi_write$delay[i], ")")))
          }
        }
        
        python_store = rbind(python_store,
                             curr_data)
        
        midi$midi_output = python_store
      }
    } else {
      midi_write$notes_vec = midi_write$`1`
      for (i in 1:nrow(midi_write)) {
        if (str_length(midi_write$notes_vec[i]) == 0) {
          curr_data = data.frame(`#command` = paste0("time.sleep(", midi_write$delay[i], ")"))
        } else {
          curr_data = data.frame(`#command` = c(paste0("keyboard.type('", midi_write$notes_vec[i], "')"),
                                                paste0("time.sleep(", midi_write$delay[i], ")")))
        }
        
        python_store = rbind(python_store,
                             curr_data)
        
        midi$midi_output = python_store
      }
    }
    
    ### OLD method for window switching (switch before every note) ----
    #if (input$multi_instrument == "Yes") {
    #  for (i in 1:nrow(midi_write)) {
    #    # Case in which there are no notes to be played
    #    if (is.na(midi_write$`1`[i]) & is.na(midi_write$`2`[i])) {
    #      curr_data = data.frame(`#command` = paste0("time.sleep(", midi_write$delay[i], ")"))
    #    } else if (!is.na(midi_write$`1`[i]) & is.na(midi_write$`2`[i])) { # Case in which instrument one has note(s) 
    #      curr_data = data.frame(`#command` = c("time.sleep(0.015)",
    #                                            paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ") # Instrument 1"),
    #                                            paste0("keyboard.type('", midi_write$`1`[i], "')"),
    #                                            paste0("time.sleep(", midi_write$delay[i], ")")))
    #    } else if (is.na(midi_write$`1`[i]) & !is.na(midi_write$`2`[i])) { # Case in which instrument two has note(s)
    #      curr_data = data.frame(`#command` = c("time.sleep(0.015)",
    #                                            paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ") # Instrument 2"),
    #                                            paste0("keyboard.type('", midi_write$`2`[i], "')"),
    #                                            paste0("time.sleep(", midi_write$delay[i], ")")))
    #    } else { # Case in which both instruments have notes
    #      curr_data = data.frame(`#command` = c(paste0("win32gui.SetForegroundWindow(", input$instrument1_id, ") # Instrument 1"),
    #                                            paste0("keyboard.type('", midi_write$`1`[i], "')"),
    #                                            "time.sleep(.015)",
    #                                            paste0("win32gui.SetForegroundWindow(", input$instrument2_id, ") # Instrument 2"),
    #                                            paste0("keyboard.type('", midi_write$`2`[i], "')"),
    #                                            paste0("time.sleep(", midi_write$delay[i], ")")))
    #    }
    #    
    #    python_store = rbind(python_store,
    #                         curr_data)
    #    
    #    midi$midi_output = python_store
    #  }
    #} else { # If not writing for multiple instruments
    #  midi_write = midi_write %>% 
    #    rowwise() %>% 
    #    mutate(`1` = ifelse(is.na(`1`), "", `1`),
    #           `2` = ifelse(is.na(`2`), "", `2`),
    #           notes_vec = paste0(`1`, `2`))
    #  
    #  for (i in 1:nrow(midi_write)) {
    #    if (str_length(midi_write$notes_vec[i]) == 0) {
    #      curr_data = data.frame(`#command` = paste0("time.sleep(", midi_write$delay[i], ")"))
    #    } else {
    #      curr_vec = midi_write$notes_vec[i]
    #      
    #      vec_split = unlist(strsplit(curr_vec, split = ""))
    #      vec_unique = unique(vec_split)
    #      vec_paste = paste0(vec_unique, collapse = "")
    #      
    #      curr_data = data.frame(`#command` = c(paste0("keyboard.type('", vec_paste, "')"),
    #                                            paste0("time.sleep(", midi_write$delay[i], ")")))
    #    }
    #    
    #    python_store = rbind(python_store,
    #                         curr_data)
    #    
    #    midi$midi_output = python_store
    #  }
    #}
    
    showModal(modalDialog(
      title = "Complete",
      "MIDI conversion complete. Click the 'Download' button to download the script (see the previous tab or the 2nd line of the outputted script for the keybinds)."
    ))
    
    shinyjs::enable("download_script")
  
  if (input$multi_instrument == "Yes") {
    if (input$instrument1_tracks != input$instrument2_tracks) {
      output$download_script <- downloadHandler(
        filename = function() {
          paste(midi$songTitle, " (Multiple instruments, different parts).py", sep = "")
        },
        content = function(file) {
          write.table(midi$midi_output,
                      file,
                      quote = FALSE,
                      row.names = FALSE,
                      col.names = FALSE)
        }
      )
    } else {
      output$download_script <- downloadHandler(
        filename = function() {
          paste(midi$songTitle, " (Multiple instruments).py", sep = "")
        },
        content = function(file) {
          write.table(midi$midi_output,
                      file,
                      quote = FALSE,
                      row.names = FALSE,
                      col.names = FALSE)
        }
      )
    }
  } else {
    output$download_script <- downloadHandler(
      filename = function() {
        paste(midi$songTitle, ".py", sep = "")
      },
      content = function(file) {
        write.table(midi$midi_output,
                    file,
                    quote = FALSE,
                    row.names = FALSE,
                    col.names = FALSE)
      }
    )
  }
}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
