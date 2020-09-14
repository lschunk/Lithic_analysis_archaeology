Summary stats - Lithic analysis Balver Höhle
================
Lisa Schunk
2020-09-14

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
dir_in <- "analysis/Balve/derived_data/"
dir_out <- "analysis/Balve/summary_stats/"
```

Raw data must be located in \~/analysis/Balve/derived\_data/.  
Formatted data will be saved in \~/analysis/Balve/summary\_stats/. The
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
1 Balve_lithic_analysis.xlsx ddea35fa68f4d8bae959bb46bb5a36ff
```

# Load data into R object

``` r
imp_data <- loadObject(paste0(dir_in, "Balve_lithic_analysis.Rbin"))
```

The imported file is:
“\~/analysis/Balve/derived\_data/Balve\_lithic\_analysis.xlsx”

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

    'data.frame':   8 obs. of  20 variables:
     $ technological.class: chr  "Keilmesser" "Keilmesser" "Keilmesser" "lateral_sharpening_spall" ...
     $ artefact.state     : chr  "complete" "Keilmesser_point" "semifinished_product" "complete" ...
     $ length.n           : num  158 21 12 110 4 3 27 12
     $ length.min         : num  5.56 18.03 42.72 12.43 20.46 ...
     $ length.max         : num  135.6 91.9 154.7 55.8 52.9 ...
     $ length.mean        : num  56.1 45.2 84.6 29.3 33.4 ...
     $ length.median      : num  52.4 43.2 72.6 27.6 30.2 ...
     $ length.sd          : num  17.08 16.58 35.21 9.18 14.31 ...
     $ width.n            : num  158 21 12 110 4 3 27 12
     $ width.min          : num  18.7 22.9 37.9 7 16.7 ...
     $ width.max          : num  81.4 53.2 91.7 29.9 20.6 ...
     $ width.mean         : num  34.1 35.6 54 17.4 18.3 ...
     $ width.median       : num  33.1 35.6 43.9 17 18 ...
     $ width.sd           : num  8.91 8.99 19.54 5.17 1.81 ...
     $ thickness.n        : num  158 21 12 110 4 3 27 12
     $ thickness.min      : num  7.35 9.53 13.6 2.14 5.66 ...
     $ thickness.max      : num  29.25 23.6 26.08 10.96 7.69 ...
     $ thickness.mean     : num  16.28 15.47 18.92 6.02 6.37 ...
     $ thickness.median   : num  15.67 15.31 18.34 5.87 6.07 ...
     $ thickness.sd       : num  4.208 3.418 4.157 2.012 0.899 ...

# Save data

## Format name of output file

``` r
file_out <- "Balve_lithic_analysis_stats"
```

The file will be saved as “\~/analysis/Balve/summary\_stats/.\[ext\]”.

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
1 Balve_lithic_analysis_stats.xlsx 3c4de53cdb3cd0bbd71629d07d5cfcd1
2 Balve_lithic_analysis_stats.Rbin f3c6a22fd1527c21d867a8b90f4e2834
```
