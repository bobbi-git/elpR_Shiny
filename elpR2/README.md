elpR2
================

## Goal

<!-- badges: start -->

<!-- badges: end -->

The goal of the elpR2 package is to provide a user interface containing
tools for processing and analyzing elephant rumble detector output. The
user interface appears like so:

<br> <img src="inst/readme_app.png" width="600"/>

## First Time Installation

You can install the development version of elpR2 like so:

1)  Download ZIP from GitHub and extract all contents

2)  Navigate to /elpR2/elpR2.Rproj and open with RStudio

3)  Install devtools and roxygen2 by typing the following command in the
    console:

``` r
install.packages(c("devtools", "roxygen2"))
```

4)  Build elpR2 package by clicking the “Install” button in the “Build”
    tab.

5)  Install elpR2 dependencies by typing the following command in the
    console:

``` r
devtools::install()
```

6)  Load in elpR2 functions by typing the following command in the
    console:

``` r
devtools::load_all()
```

7)  Open shiny app by typing the following command in the console:

``` r
elpRApp()
```

<br> <img src="inst/readme_instructions.png" width="1000"/>

## Usage

Follow installation steps 2, 4, 6, and 7 each time you want to run the
app. The elpR2 functions can be run from the app. Output files from the
functions are placed in the “files_for_elpR” folder, unless indicated
otherwise. IMPORTANT: Do not change the folder organization structure!

## Available Functions

``` r
elpRApp()
sound_check_function()
exclude_sounds_function()
restructure_rumble_function()
restructure_gunshot_function()
restructure_general_function()
data_summaries_function()
detector_performance_function()
maps_function()
```

`detector_performance_function()` compares detector selection tables with
truth selection tables only during the site-specific periods in a tab-delimited
schedule. Its schedule columns are `Begin Date`, `Begin Time`, `End Date`,
`End Time`, and `Site`. Tables must include `Begin File`, `Begin Time (s)`,
`End Time (s)`, and `File Offset (s)`; detector tables must also include a
numeric score column (default: `Score`). Recording filenames must include a
`YYYYMMDD_HHMMSS` timestamp. The function returns precision, recall, F1, and
false positives per scheduled recording hour for the requested score
thresholds.

When run from section 7 of the Shiny app, choose an output folder. Each
analysis creates a uniquely named `detector_performance_*` subfolder there
containing tab-delimited `metrics.tsv`, `truth_events.tsv`,
`detector_events.tsv`, and `schedule.tsv` files.

For example, after loading the truth and detector tables into lists:

``` r
performance <- detector_performance_function(
  truth_tables,
  det_tables,
  schedule_path = "path/to/schedule.txt",
  score_column = "Confidence",
  score_thresholds = seq(0.1, 0.99, by = 0.01)
)
performance$metrics
```

Function documentation for elpR2 can be viewed in the “HELP” tab within
the app.

## Example Datasets

This package provides example datasets to practice using elpR2.
