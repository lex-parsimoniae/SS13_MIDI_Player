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
  library(DescTools)
  #library(webexercises)
  library(zoo)
  library(tuneR)
  library(DT)
  library(xlsx)
  library(openxlsx)
  library(writexl)
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
      tags$p("This R Shiny web application takes as input uploaded MIDI files and outputs Python scripts that simulate keypresses on virtual instruments (1st tab), or to code that can be used with the player pianos (2nd tab). This app was designed with the Goonstation branch of SS13 in mind, 
             but should theoretically be compatible with any virtual instrument that allows one to customize keypresses. To run the generated Python scripts (for the keyboard instruments only), you will need a local installation of ", a(href = 'https://www.python.org/downloads/', 'Python', .noWS = "outside"), " and the ", a(href = 'https://pypi.org/project/pynput/', 'pynput', .noWS = "outside"),  " module.
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
        "instrument",
        "Instrument to generate script for. Note that some instruments have restricted ranges. For these instruments, notes outside the range will be 'compressed' to the instrument's available range (e.g., for an instrument with a lower bound of C3, C1 and C2 will be converted to C3)",
        choices = c("Piano (C2 - C7)",
                    "Banjo (E3 - C6)",
                    "Trumpet (E3 - C6)",
                    "Saxophone (G3 - C6)",
                    "Fiddle (A3 - G6)",
                    "Electric Guitar (E2 - C6)",
                    "Acoustic Guitar (D2 - C6)",
                    "Electric Bass (D1 - D4)"),
        selected = "Piano (C2 - C7)",
        choiceNames = c("Piano (C2 - C7)",
                        "Banjo (E3 - C6)",
                        "Trumpet (E3 - C6)",
                        "Saxophone (G3 - C6)",
                        "Fiddle (A3 - G6)",
                        "Electric Guitar (E2 - C6)",
                        "Acoustic Guitar (D2 - C6)",
                        "Electric Bass (D1 - D4)")
      ),
      numericInput("tempo_adjust",
                   "Tempo modifier",
                   value = 1,
                   min = .01),
      bsTooltip("tempo_adjust",
                "Multiplier to apply to the default tempo (e.g., 1.5 = 150% faster)",
                placement = "left",
                trigger = "hover"),
      actionButton(
        "convert_start",
        "Convert MIDI",
        width = '100%',
        class = "btn-success"),
      downloadButton("download_script", "Download")
  )
)

### Player piano tab ----
convert_tab_player <- tabItem(
  tabName = "convert_tab_player",
  box(width = 12,
      collapsible = FALSE,
      title = "Convert midi to Player Piano input",
      tags$p('Takes a MIDI file and outputs a .csv file where the rows represent different player pianos (a song that exceeds the player piano character limit will need to be input as separate signals or additional pianos to play the entire song, e.g., via MechComp). Supports concurrent note playing, so each additional player piano will need to be set up to play sequentially, not simultaneously. Does not require any additional software.'),
      tags$br(),
      tags$b('NOTE: Percussion tracks and other tracks without notes do not play well with the converter, so make sure to remove those from the MIDI before trying to convert! Also, if a MIDI just refuses to sound right no matter the delay you set (e.g., ends up with super-long delays) one thing that I have found works is opening the downloaded MIDI in ', a(href = 'https://musescore.org/en/download', 'MuseScore', .noWS = "outside"), ' and re-exporting the file to MIDI.'),
      tags$br(),
      fileInput(
        "midi_upload_player",
        "Upload your MIDI",
        multiple = FALSE,
        accept = c(".mid", ".midi")),
      numericInput("lcd",
                   "Fastest MIDI delay to use (optional, measured in ticks)",
                   value = NA,
                   min = 0),
      bsTooltip("lcd",
                "The program will attempt to auto-detect the lowest common denominator in the delays between notes that is divisible by 10 (i.e., excluding glissandos). Usually works, but if it ends up sounding wonky, you might need to manually change this. From my testing, 60, 80 and 120 seem to work OK for most songs.",
                placement = "left",
                trigger = "hover"),
      actionButton(
        "convert_start_player",
        "Convert MIDI",
        width = '100%',
        class = "btn-success"),
      downloadButton("download_script_player", "Download")
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
      menuItem("MIDI -> Keypresses", tabName = "convert_tab", icon = icon("music")),
      menuItem("MIDI -> Player Piano", tabName = "convert_tab_player", icon = icon("music"))
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
      convert_tab,
      convert_tab_player
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyjs::disable("download_script")
  #shinyjs::disable("download_script_player")
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  midi = reactiveValues(midi_input = NULL,
                        midi_output = NULL,
                        midi_key_player = NULL,
                        test_midi = NULL,
                        all_strings = NULL,
                        songTitle = NULL,
                        note_delay = NULL,
                        songTitlePlayer = NULL,
                        sheet_store = NULL)
  
  output$instruments_pic = renderImage({
    list(src = "www/img/instruments.png",
         width = "315px",
         height = "303px",
         alt = "Instruments")
  }, deleteFile = FALSE)
  
  # Set the keybinds ----
  ## Electric Bass ----
  keybinds_bass = data.frame(key = c("!", "@", "#", "$", "%", "^", "&", "*", "(", ")", 
                                     "Q", "W", "E", "R", "T", "Y", "U", "I", 
                                     "O", "P", "A", "S", "D", "F", "G", "H",
                                     "J", "K", "L", "Z", "X", "C", "V", "B",
                                     "q", "w", "e"),
                             new_note = c("D1", "D#1", "E1", "F1", "F#1", "G1", "G#1", "A1", "A#1", "B1",
                                          "C2", "C#2", "D2", "D#2", "E2", "F2", "F#2", "G2", "G#2", "A2", "A#2", "B2",
                                          "C3", "C#3", "D3", "D#3", "E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3",
                                          "C4", "C#4", "D4"))
  
  keybinds_bass_paste = keybinds_bass %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
  ## Acoustic guitar ----
  keybinds_acoustic = data.frame(key = c("E", "R", "T", "Y", "U", "I", 
                                         "O", "P", "A", "S", "D", "F", "G", "H",
                                         "J", "K", "L", "Z", "X", "C", "V", "B",
                                         "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                         "a", "s", "d", "f", "g", "h", "j", "k", "l",
                                         ";", "z", "x", "c", "v", "b"),
                                 new_note = c("D2", "D#2", "E2", "F2", "F#2", "G2", "G#2", "A2", "A#2", "B2",
                                              "C3", "C#3", "D3", "D#3", "E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3",
                                              "C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                              "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                                              "C6"))
  
  keybinds_acoustic_paste = keybinds_acoustic %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
  ## Electric guitar ----
  keybinds_guitar = data.frame(key = c("T", "Y", "U", "I", 
                                       "O", "P", "A", "S", "D", "F", "G", "H",
                                       "J", "K", "L", "Z", "X", "C", "V", "B",
                                       "q", "w", "e", "r", "t", "y", "u", "i", "o", "p",
                                       "a", "s", "d", "f", "g", "h", "j", "k", "l",
                                       ";", "z", "x", "c", "v", "b"),
                               new_note = c("E2", "F2", "F#2", "G2", "G#2", "A2", "A#2", "B2",
                                            "C3", "C#3", "D3", "D#3", "E3", "F3", "F#3", "G3", "G#3", "A3", "A#3", "B3",
                                            "C4", "C#4", "D4", "D#4", "E4", "F4", "F#4", "G4", "G#4", "A4", "A#4", "B4",
                                            "C5", "C#5", "D5", "D#5", "E5", "F5", "F#5", "G5", "G#5", "A5", "A#5", "B5",
                                            "C6"))
  
  keybinds_guitar_paste = keybinds_guitar %>% 
    select(key) %>% 
    as.list() %>% 
    unlist() %>% 
    paste0(collapse = "")
  
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
                   "Fiddle",
                   "Electric Guitar",
                   "Acoustic Guitar",
                   "Electric Bass"),
    Keybinds = c(keybinds_piano_paste,
                 keybinds_banjo_paste,
                 keybinds_banjo_paste,
                 keybinds_sax_paste,
                 keybinds_violin_paste,
                 keybinds_guitar_paste,
                 keybinds_acoustic_paste,
                 keybinds_bass_paste),
    Copy = shinyInput(actionButton, 8,
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
    } else if (input$instrument == "Trumpet (E3 - C6)") { # Trumpet
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
    } else if (input$instrument == "Electric Guitar (E2 - C6)") { # Electric Guitar
      keybinds = keybinds_guitar
      midi$instrumentName = "Electric Guitar"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 2, 2,
                               ifelse(octave > 5 & note != "C", 5, octave))) %>% 
        mutate(octave = ifelse(octave > 5 & note == "C", 6, octave),
               new_note = paste0(note, octave)) %>%
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    } else if (input$instrument == "Acoustic Guitar (D2 - C6)") { # Acoustic Guitar
      keybinds = keybinds_acoustic
      midi$instrumentName = "Acoustic Guitar"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 2, 2,
                               ifelse(octave > 5 & note != "C", 5, octave))) %>% 
        mutate(octave = ifelse(octave > 5 & note == "C", 6, octave),
               new_note = paste0(note, octave)) %>%
        left_join(keybinds) %>% 
        mutate(number = as.numeric(number))
    } else if (input$instrument == "Electric Bass (D1 - D4)") { # Electric Bass
      keybinds = keybinds_bass
      midi$instrumentName = "Electric Bass"
      
      midi_key = read.csv("www/Midi note key.csv",
                          fileEncoding = "UTF-8-BOM") %>% 
        gather(key = "note",
               value = "number",
               -Octave) %>% 
        rename("octave" = Octave) %>% 
        mutate(octave = octave + 1) %>% 
        mutate(note = str_replace(note, "\\.", "#"),
               octave = ifelse(octave < 1, 1,
                               ifelse(octave > 4, 4, octave))) %>% 
        mutate(new_note = paste0(note, octave)) %>%
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
    
    message("Read in MIDI #2")
    
    ticks_per_beat = 480
    
    midi_processed = input_midi %>% 
      select(time, event, parameter1) %>% 
      rename("type" = event,
             "number" = parameter1) %>% 
      filter(grepl("Note On", type)) %>%
      mutate(number = as.numeric(number)) %>% 
      left_join(midi_key) %>% 
      group_by(time) %>% 
      summarise(notes_vec = paste(key, collapse = "")) %>% 
      distinct() %>% 
      ungroup() %>% 
      mutate(delay_after = NA)
    
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
      group_by(time) %>% 
      summarise(notes_vec = paste(notes_vec, collapse = ""),
                tempo = as.numeric(tempo)) %>% 
      distinct() %>% 
      ungroup() %>% 
      mutate(delay_after = NA)
    
    ### Remove repeats in the strings
    for (i in 1:nrow(midi_processed)) {
      curr_vec = midi_processed$notes_vec[i]
      
      vec_split = unlist(strsplit(curr_vec, split = ""))
      vec_unique = unique(vec_split)
      vec_paste = paste0(vec_unique, collapse = "")
      
      midi_processed$notes_vec[i] = vec_paste
    }
    
    ### Add beat delays ----
    for (i in 1:nrow(midi_processed)) {
      if (midi_processed$time[i] < max(midi_processed$time)) {
        midi_processed$delay_after[i] = 
          midi_processed$time[i+1] - midi_processed$time[i]
      } else {
        midi_processed$delay_after[i] = 0
      }
    }
    
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
      mutate(delay = delay * (1 / input$tempo_adjust))
    
    ## Write to .py ----
    python_store = data.frame(`#command` = c(paste0("# Keybinds = ", keybinds_paste),
                                             "import time",
                                             "from pynput.keyboard import Controller",
                                             "keyboard = Controller()",
                                             "time.sleep(5)"))
    
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
    
    showModal(modalDialog(
      title = "Complete",
      "MIDI conversion complete. Click the 'Download' button to download the script (see the previous tab or the 2nd line of the outputted script for the keybinds)."
    ))
    
    shinyjs::enable("download_script")
  })
  
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
  
  # Piano player converter ----
  observeEvent(input$midi_upload_player, {
    midi$midi_key_player = read.csv("www/Midi note key.csv",
                               fileEncoding = "UTF-8-BOM") %>% 
      gather(key = "note",
             value = "number",
             -Octave) %>% 
      rename("octave" = Octave) %>% 
      mutate(octave = octave + 1) %>% 
      mutate(note = str_replace(note, "\\.", "#"),
             octave = case_when(octave < 1 & note %nin% c("A", "B") ~ 1,
                                octave > 7 & note != "C" ~ 7,
                                octave > 8 & note == "C" ~ 8,
                                .default = octave),
             new_note = paste0(note, octave)) %>% 
      #mutate(new_note = ifelse(new_note == "C8" | new_note == "C9", "C7", new_note)) %>%
      mutate(parameter1 = as.numeric(number)) %>% 
      select(parameter1, new_note)
    
    midi$test_midi = readMidi(input$midi_upload_player$datapath)
    
    message("Read midi for conversion to player piano")
  })
  
  observeEvent(input$convert_start_player, {
    shinyjs::disable("download_script_player")
    req(input$midi_upload_player)
    
    test_midi = midi$test_midi
    midi_key_player = midi$midi_key_player
    
    message("Retrieved midi object")
    
    songTitlePlayer = gsub("_", " ", input$midi_upload_player)
    midi$songTitlePlayer = gsub(".mid", "", songTitlePlayer)
    
    showModal(modalDialog(title = "Conversion in progress", "Converting, please wait"))
    #------#
    ## Set the lowest common denominator (i.e., the most common distance between notes) ----
    delays = test_midi %>% 
      filter(grepl("Note On", event)) %>% 
      select(time) %>% 
      distinct() %>% 
      arrange(time)
    
    for (i in 1:nrow(delays)) {
      delays$difference[i] = delays$time[i+1]-delays$time[i]
    }
    
    message(delays)
    message("Got delays")
    
    ## Get the BPM ----
    bpm = test_midi %>% 
      filter(event == "Set Tempo") %>% 
      slice_head(n = 1) %>% 
      select(parameterMetaSystem) %>% 
      as.numeric()
    
    bpm = 60000000/bpm
    bps = bpm / 60
    tpb = 480
    tps = tpb * bps
    
    ## Reformat time (divided by lcd) ----
    ### Get the lowest common denominator 
    unique_diffs = as.data.frame(unique(delays$difference)) %>% 
      `colnames<-` (c("unique_diffs")) %>% 
      filter(unique_diffs %% 10 == 0) %>% 
      as.list()
    
    message(unique_diffs)
    
    unique_diffs_data = as.data.frame(unique_diffs)
    
    if (is.na(input$lcd)) {
      for (m in 1:nrow(unique_diffs_data)) {
        curr_diff = unique_diffs_data$unique_diffs[m]
        
        curr_notes_per_second = tps / curr_diff
        curr_note_delay = 1 / curr_notes_per_second
        unique_diffs_data$curr_note_timing[m] = abs(10 - (100 * curr_note_delay))
        
      }
      
      message(unique_diffs_data)
      
      lcd = arrange(unique_diffs_data,
                    curr_note_timing) %>% 
        ungroup() %>% 
        filter(curr_note_timing < 5) 
      
      if (nrow(lcd) == 0) {
        lcd = unique_diffs_data %>% 
          filter(unique_diffs == min(unique_diffs)) %>% 
          select(unique_diffs) %>% 
          as.numeric()
      } else {
        lcd = lcd %>% 
          filter(unique_diffs == min(unique_diffs)) %>% 
          select(unique_diffs) %>% 
          as.numeric()
      }
    } else {
      lcd = as.numeric(input$lcd)
    }
    
    message(paste0("LCD: ", lcd))
    
    notes_per_second = tps / lcd
    note_delay = 1 / notes_per_second
    note_timing = round(100 * note_delay)
    
    if (note_timing < 10) {
      note_timing = 10
    } else {
      note_timing = note_timing
    }
    
    message("Calculated timing measures")
    
    midi$note_timing = note_timing
    
    test_midi_processed = test_midi %>% 
      filter(grepl("Note On", event)) %>% 
      left_join(midi_key_player) %>% 
      mutate(time_adj = time/lcd) %>% 
      mutate(remainder = time_adj %% 1) %>% 
      filter(remainder == 0) %>% 
      group_by(time_adj) %>% 
      mutate(piano = 1:n()) %>% 
      ungroup() %>% 
      arrange(time_adj)
    
    ## Get the total # of pianos needed ----
    #n_pianos = test_midi_processed %>% 
    #  group_by(time_adj) %>% 
    #  count() %>% 
    #  arrange(desc(n)) %>% 
    #  ungroup() %>% 
    #  slice_head(n = 1) %>% 
    #  select(n) %>% 
    #  as.numeric()
    #
    #list_pianos = c(1:n_pianos)
    
    #all_times = seq(from = 0, to = max(test_midi_processed$time_adj))
    
    char_limit = 15360 - 10
    
    #chars_needed = max(all_times)*8
    #
    #sections_needed = ceiling(chars_needed/char_limit)
    #
    #sections_list = data.frame("section" = 1:sections_needed) %>% 
    #  mutate(max_chars = section*char_limit)
    #
    ### Fill information for each beat and format for the piano players ----
    #all_notes = expand_grid(all_times,
    #                        list_pianos) %>% 
    #  `colnames<-` (c("time_adj", "piano")) %>% 
    #  left_join(test_midi_processed %>% 
    #              select(time_adj,
    #                     piano,
    #                     new_note)) %>% 
    #  mutate(new_note = ifelse(is.na(new_note), "R1", new_note)) %>% 
    #  mutate(octave = parse_number(new_note),
    #         accidentals = ifelse(grepl("b", new_note, ignore.case = FALSE), "B", 
    #                              ifelse(grepl("#", new_note), "S", "N")),
    #         notename = str_extract(new_note, "[A-Z]+"),
    #         dynamic = ifelse(new_note == "R", "R", "N")) %>% 
    #  mutate(accidentals = ifelse(notename == "R", "R", accidentals),
    #         octave = ifelse(notename == "R", "R", octave)) %>% 
    #  mutate(formatted = paste0(notename, ",", accidentals, ",", dynamic, ",", octave, "|")) %>% 
    #  ungroup() %>% 
    #  mutate(time_adj = time_adj + 1,
    #         chars = time_adj*8,
    #         cut = ceiling(chars/char_limit)) %>% 
    #  ungroup() %>% 
    #  arrange(piano,
    #          time_adj)
    
    all_notes = test_midi_processed %>% 
      select(time_adj,
             new_note) %>% 
      mutate(new_note = ifelse(is.na(new_note), "R1", new_note))
    
    ## Create delays ----
    for (i in 1:nrow(all_notes)) {
      all_notes$delay[i] = all_notes$time_adj[i+1] - all_notes$time_adj[i]
    }
    
    ## Format strings ----
    all_notes = all_notes %>% 
      mutate(octave = parse_number(new_note),
             accidentals = ifelse(grepl("b", new_note, ignore.case = FALSE), "B", 
                                  ifelse(grepl("#", new_note), "S", "N")),
             notename = str_extract(new_note, "[A-Z]+"),
             dynamic = ifelse(new_note == "R", "R", "N")) %>% 
      mutate(accidentals = ifelse(notename == "R", "R", accidentals),
             octave = ifelse(notename == "R", "R", octave)) %>% 
      mutate(formatted = paste0(notename, ",", accidentals, ",", dynamic, ",", octave, ",", delay, "|")) %>% 
      ungroup() %>% 
      mutate(time_adj = time_adj + 1,
             row_number = 1:n(),
             chars = row_number*10,
             cut = ceiling(chars/char_limit)) %>% 
      ungroup() %>% 
      arrange(time_adj)
    
    ### For each piano, generate a single string of inputs ----
    all_strings = all_notes %>%
      group_by(cut) %>% 
      summarize(notestring = paste0(formatted, collapse = "")) %>% 
      rowwise() %>% 
      mutate(notestring = paste0("timing,", note_timing, "|", notestring)) %>% 
      ungroup() %>% 
      rename("Piano" = cut,
             "Notestring" = notestring)
    
    #### Create workbook to store the output ----
    #wb = createWorkbook()
    #
    #for (h in 1:length(unique(all_strings$cut))) {
    #  curr_string_data = filter(all_strings,
    #                            cut == unique(all_strings$cut)[h]) %>% 
    #    select(-cut)
    #  
    #  addWorksheet(wb, sheetName = paste0("Signal ", h))
    #  
    #  writeData(wb, sheet = paste0("Signal ", h), x = curr_string_data)
    #}
    #
    midi$sheet_store = all_strings
    
    ### Find out how many signals will need to be sent ----
    #bar_limit = floor(char_limit/5)
    #
    #total_length = as.numeric(nchar(all_strings$notestring[1]))
    #
    #n_signals = ceiling(total_length/char_limit)
    #
    #breaks = seq(from = 0,
    #             to = total_length,
    #             by = total_length/n_signals)
    #
    #final_time = max(test_midi_processed$time)
    #------#
    showModal(modalDialog(
      title = "Complete",
      "MIDI conversion complete. Click the 'Download' button to download the output. The filename of the download will include the recommended piano player note delay."
    ))
    
    shinyjs::enable("download_script_player")
    
  })
  
  output$download_script_player <- downloadHandler(
    filename = function() {
      paste0(midi$songTitlePlayer, " (Note delay = ", midi$note_timing, ").csv")
    },
    content = function(file) {
      write.csv(midi$sheet_store,
                  file,
                  quote = TRUE,
                  row.names = FALSE,
                  col.names = FALSE,
                sep = "")
    }
    )
}

# Run the application
shinyApp(ui = ui, server = server)
