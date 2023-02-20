# epicodr <img src="man/figures/logo.png" align="right" width="100" />
Import, primary coding and export functions for data of the National Pandemic Cohort Network [NAPKON](https://www.napkon.de). 

## :floppy_disk: Versions & Download

The latest versions of epicodr can be found here: 
[Releases](https://github.com/nukleus-ecu/epicodr/releases)

## :hammer: How to use this package

Using this package, you can import SÜP, HAP and POP data exported from SecuTrial by the Transfer-Office, set variable and value labels, format date variables, perform primary coding, as well as export the data, e.g. in SPSS format.

### Tutorial
You can find a tutorial on how to use this package on YouTube at [NUKLEUS ECU](https://www.youtube.com/watch?v=WAY-DGsRYqU).

### Step-by-Step Guide

1. Install a R runtime environment and git on your computer. Provide Internet access.
2. Create a R file and put the following code inside:
```r
# Install package remotes
if(!require(remotes)) install.packages("remotes")

# Install package epicodr
# replace "release" with a specific version, if necessary
# See section *Versions* for more details
remotes::install_github("nukleus-ecu/epicodr@*release")

# Load package epicodr
library(epicodr)

# Specifiy location where to find data zip file
zip_file_path <- "data/import/NAME_OF_EXPORT_FILE.zip"

# Import the zip file data
data <- zip_file_path %>% 
  read_tsExport(separator = ";", decimal = ",")

# Execute primary coding on data
data_primary_coded <- data %>% 
  # place coding function accordingly, e.g., primary_coding_suep() or primary_coding_hap() or primary_coding_pop()
  primary_coding_suep()

# Note: SPSS only supports levels of variables with <= 120 characters
# Set affected variables to NULL
# e.g.
data_primary_coded$fuv3$disab_ecog.factor <- NULL

# Export the prepared data in R to formats of other statistical software e.g. SPSS (sav) to export folder
# For other formats replace "sav" with (one of "dta", "sas", "sav", "xpt")
write_tsExport(data_primary_coded, format = "sav", path = "data/export/", metadata = TRUE)
```
3. Run the script.
4. Data is exported to folder [data/export](data/export).


### Alternative: Clone this Repository

1. Install a R runtime environment and git on your computer. Provide Internet access.
2. Clone this repository: ```git clone https://github.com/nukleus-ecu/epicodr.git```
3. Export data from secuTrial and place the *.zip file in [data/import/](data/import).
4. Create a new R script and add previously shown code for sample import, primary coding and SPSS export or create a new R script by yourself.
5. Data is exported to folder [data/export](data/export).


## :pencil2: Supported data structure

Descriptions of the primary codings of the NAPKON cohort platforms SÜP, POP and HAP are available at the following link:

[https://cloud.napkon.de/s/kgSLW7gkBExBYfC?path=%2FPrim%C3%A4rkodierung%20Datenauswertung%20_%20Primary%20coding%20Data%20analysis](https://cloud.napkon.de/s/kgSLW7gkBExBYfC?path=%2FPrim%C3%A4rkodierung%20Datenauswertung%20_%20Primary%20coding%20Data%20analysis)

Additionally, there are manuals that include variable categorizations, aids for calculating scores, defining normal ranges of laboratory parameters and clinical parameters. Further explanations are also available in the tutorial presentation. 


## Repository structure

This repository is structured as follows:
- [R](R) contains all R code
- [man](man) contains all manuals to ecu-functions

### :computer: R code structure

The R code is structured as follows:
- [R](R) contains platform independent and platform specific implementations:
    - [read-tsExport.R](R/write_tsExport.R) contains import functions
    - [write-tsExport.R](R/write_tsExport.R) contains export functions, e.g. for export to SPSS (.sav)
    - [primary-coding.R](R/primary-coding.R) contains platform independent helper functions used in platform specific primary coding scripts
    - [primary-coding-suep.R](R/primary-coding-suep.R) contains platform specific functions for primary coding for platform SUEP
    - [primary-coding-hap.R](R/primary-coding-hap.R) contains platform specific functions for primary coding for platform HAP
    - [primary-coding-pop.R](R/primary-coding-pop.R) contains platform specific functions for primary coding for platform POP
