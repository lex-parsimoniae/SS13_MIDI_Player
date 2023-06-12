# SS13 MIDI Player

<p align="center">
  <img width="210" height="202" src="https://github.com/lex-parsimoniae/SS13_MIDI_Player/blob/main/app/Goonstation_Instrument_Player/www/img/instruments.png">
</p>

This R Shiny web application takes as input uploaded MIDI files and outputs Python scripts that simulate keypresses on virtual instruments. This app was designed with the Goonstation branch of SS13 (and its instruments) in mind, but should theoretically be compatible with any virtual instrument that allows one to customize keybinds. 

To run the generated scripts, you will need a local installation of [Python](https://www.python.org/downloads/) and the 'pynput' module. Once you've installed Python, 'pynput' can be installed by opening the terminal/command prompt and executing the following command:
```
pip install pynput
```
(See [this link](https://docs.python.org/3/installing/index.html) for more info on installing Python modules)

The [Example songs](https://github.com/lex-parsimoniae/SS13_MIDI_Player/tree/main/Example%20songs) folder has a couple sample scripts generated by the program.

These scripts don't have inbuilt functionality for pausing, but I use [AutoHotKey](https://www.autohotkey.com/v2/) to bind .cmd/.bat files that suspend the Python process (sourced from [here](https://github.com/craftwar/suspend)) to keybinds (f8 to pause, f9 to resume). To use this functionality, simply install AutoHotKey (v2), download and extract the files in the [Script pausing](https://github.com/lex-parsimoniae/SS13_MIDI_Player/tree/main/Script%20pausing) folder (make sure all the files are in the same place), and open 'Keyboard pause - resume.ahk' in the background. Other than that, once you have the script, simply paste the keybinds into the instrument, open the script (it has a 5s buffer time), and tab into the instrument.

**Run the converter online [here](https://lex-parsimoniae.shinyapps.io/Goonstation_Instrument_MIDI_converter/). If the online version is not working, or if you'd prefer to run locally, the steps to do so are:**

1. Download and install [R](https://www.r-project.org/) and [RStudio](https://posit.co/download/rstudio-desktop/)
2. Download all the files in this repository via the "Code" button ("Download ZIP")
3. Extract the downloaded files wherever
4. In the extracted folder, open "Goonstation_Instrument_Player.Rproj" in RStudio
5. Once in RStudio, open the R script file "R package installation.R" (you'll only have to do this once)
6. Run everything in this file to install the necessary R packages
7. Now, still in RStudio, navigate to the "app/Goonstation_Instrument_Player" folder
8. Open "app.R"
9. Hit "Run App" in the top right corner
10. That's it!

**Keybinds for instruments are as follows:**

```
Piano: QWERTYUIOPASDFGHJKLZXCVBqwertyuiopasdfghjkl;zxcvbnm1234567890

Banjo: tyuiopasdfghjkl;zxcvbnm1234567890

Trumpet: tyuiopasdfghjkl;zxcvbnm1234567890

Saxophone: qwertyuiopasdfghjkl;zxcvbnm123

Fiddle: qwertyuiopasdfghjkl;zxcvbnm12345678
```