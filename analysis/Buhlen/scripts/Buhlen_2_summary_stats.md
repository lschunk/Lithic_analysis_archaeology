Summary stats - Lithic analysis Buhlen
================
Lisa Schunk
2020-09-17

  - [Goal of the script](#goal-of-the-script)
  - [Load packages](#load-packages)
  - [Get names, path and information of the
    file](#get-names-path-and-information-of-the-file)
  - [Load data into R object](#load-data-into-r-object)
  - [Define numeric variables](#define-numeric-variables)
  - [Compute summary statistics](#compute-summary-statistics)
      - [Create function to compute the statistics at
        once](#create-function-to-compute-the-statistics-at-once)
      - [Compute the summary
        statistics](#compute-the-summary-statistics)
          - [Dimensions](#dimensions)
  - [Save data](#save-data)
      - [Format name of output file](#format-name-of-output-file)
      - [Write to XLSX](#write-to-xlsx)
      - [Save R object](#save-r-object)
      - [Show file information](#show-file-information)

-----

# Goal of the script

This script computes standard descriptive statistics for each group.  
The groups are based on:

  - Tool type
  - state of the tool (Complete, distal/proximal fragment, medial
    fragment)

It computes the following statistics:

  - n (sample size = `length`): number of measurements  
  - smallest value (`min`)  
  - largest value (`max`)
  - mean  
  - median  
  - standard deviation (`sd`)

<!-- end list -->

``` r
dir_in <- "analysis/Buhlen/derived_data/"
dir_out <- "analysis/Buhlen/summary_stats/"
```

Raw data must be located in \~/analysis/Buhlen/derived\_data/.  
Formatted data will be saved in \~/analysis/Buhlen/summary\_stats/. The
knit directory for this script is the project directory.

-----

# Load packages

``` r
library(openxlsx)
library(R.utils)
library(tools)
library(doBy)
```

-----

# Get names, path and information of the file

``` r
data_file <- list.files(dir_in, pattern = "\\.xlsx$", full.names = TRUE)
md5_in <- md5sum(data_file)
info_in <- data.frame(file = basename(names(md5_in)), checksum = md5_in, row.names = NULL)
```

The checksum (MD5 hashes) of the imported file is

``` 
                         file                         checksum
1 Buhlen_lithic_analysis.xlsx 6bbb525f4e13792e5ad6f7f7c1ff464b
```

# Load data into R object

``` r
imp_data <- loadObject(paste0(dir_in, "Buhlen_lithic_analysis.Rbin"))
```

The imported file is:
“\~/analysis/Buhlen/derived\_data/Buhlen\_lithic\_analysis.xlsx”

-----

# Define numeric variables

``` r
# changes the order of columns 
imp_data <- imp_data[c(1:22, 27,23:26, 28:34)]
num.var <-19:23
```

The following variables will be used:

    [19] length
    [20] width
    [21] thickness
    [22] weight
    [23] thickness.back

-----

# Compute summary statistics

## Create function to compute the statistics at once

``` r
nminmaxmeanmedsd <- function(x){
    y <- x[!is.na(x)]
    n_test <- length(y)
    min_test <- min(y)
    max_test <- max(y)
    mean_test <- mean(y)
    med_test <- median(y)
    sd_test <- sd(y)
    out <- c(n_test, min_test, max_test, mean_test, med_test, sd_test)
    names(out) <- c("n", "min", "max", "mean", "median", "sd")
    return(out)
}
```

## Compute the summary statistics

### Dimensions

``` r
# Dimensions Keilmesser, Keilmesser-points, Pradnick scraper, scraper & Later sharpening spall 
dimensions <- summaryBy(length + width + thickness ~ technological.class + artefact.state,
                  data=imp_data, FUN=nminmaxmeanmedsd)
str(dimensions)
```

    'data.frame':   11 obs. of  20 variables:
     $ technological.class: chr  "hammerstone" "Keilmesser" "Keilmesser" "Keilmesser" ...
     $ artefact.state     : chr  "complete" "complete" "distal_fragment" "Keilmesser_point" ...
     $ length.n           : num  1 111 1 15 2 1 36 3 3 24 ...
     $ length.min         : num  90 30 22 13 25 82.6 21 35 26 27 ...
     $ length.max         : num  90 114 22 46 54 ...
     $ length.mean        : num  90 53 22 28.1 39.5 ...
     $ length.median      : num  90 50 22 28 39.5 ...
     $ length.sd          : num  NA 15.56 NA 8.65 20.51 ...
     $ width.n            : num  1 111 1 15 2 1 36 3 3 24 ...
     $ width.min          : num  62 14 44 19 38 ...
     $ width.max          : num  62 71.9 44 42 54 ...
     $ width.mean         : num  62 32.9 44 30 46 ...
     $ width.median       : num  62 32 44 30 46 ...
     $ width.sd           : num  NA 8.57 NA 6.63 11.31 ...
     $ thickness.n        : num  1 111 1 15 2 1 36 3 3 24 ...
     $ thickness.min      : num  52 7 12 8 10 ...
     $ thickness.max      : num  52 31 12 20 26 ...
     $ thickness.mean     : num  52 16.1 12 12.6 18 ...
     $ thickness.median   : num  52 15 12 13 18 ...
     $ thickness.sd       : num  NA 4.56 NA 3.18 11.31 ...

# Save data

## Format name of output file

``` r
file_out <- "Buhlen_lithic_analysis_stats"
```

The file will be saved as “\~/analysis/Buhlen/summary\_stats/.\[ext\]”.

## Write to XLSX

``` r
write.xlsx(list(dimensions = dimensions), 
           file = paste0(dir_out, file_out, ".xlsx"))
```

## Save R object

``` r
saveObject(list(dimensions = dimensions), 
           file = paste0(dir_out, file_out, ".Rbin"))
```

## Show file information

``` r
file_out <- c(paste0(dir_out, file_out, ".xlsx"), paste0(dir_out, file_out, ".Rbin"))
md5_out <- md5sum(file_out)
info_out <- data.frame(files = basename(names(md5_out)), checksum = md5_out, row.names = NULL)
```

The checksum (MD5 hashes) of the exported files are:

``` 
                              files                         checksum
1 Buhlen_lithic_analysis_stats.xlsx 55b3b7c11e734b4bcd6a553936675afa
2 Buhlen_lithic_analysis_stats.Rbin 811aecaf15829412378f7ede7983a335
```
