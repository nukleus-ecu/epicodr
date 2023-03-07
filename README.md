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
  
# Prepare data for SPSS export (remove .factor variables, add value labels, 
# optionally remove original date variables and set default missing values)
# - Set rm_stdates = FALSE if you want to keep original secutrial date variables
# - set_missings = TRUE, assign general missing values for the whole data set. 
#   Type "missing_labels_default" into console (without quotes) in order to 
#    check default values that are set to missing.
# - Additional Options: 
#   - keep_date_vars: character vector of date variable names that should be 
#     kept when rm_stdates = TRUE. Default: "birthdate_score"
#   - missing_labels: character vector that defines variable labels that should 
#     be set as missing value. Default: missing_labels_default
#   - missing_values: numeric vector that defines variable values that should be 
#     set as missing value. Default: NULL
data_spss_export_prepared <- prepare_spss_export(data_primary_coded, 
                                                 rm_stdates = TRUE, 
												 set_missings = TRUE)

# Export the prepared data in R to formats of other statistical software e.g. SPSS (sav) to export folder
# For other formats replace 'data_spss_export_prepared' with 'data_primary_coded' and "sav" with (one of "dta", "sas", "sav", "xpt")
write_tsExport(data_spss_export_prepared, format = "sav", path = "data/export/", metadata = TRUE)

```
3. Run the script.
4. Data is exported to folder [data/export](data/export).


### Alternative: Clone this Repository

1. Install a R runtime environment and git on your computer. Provide Internet access.
2. Clone this repository: ```git clone https://github.com/nukleus-ecu/epicodr.git```
3. Export data from secuTrial and place the *.zip file in [data/import/](data/import).
4. Open and run script [R/run/sample.R](R/run/sample.R) for sample import, primary coding and SPSS export or create a new R script by yourself.
5. Data is exported to folder [data/export](data/export).


## :pencil2: Supported data structure

Descriptions of the primary codings of the NAPKON cohort platforms SÜP, POP and HAP are available at the following link:

[https://cloud.napkon.de/s/kgSLW7gkBExBYfC?path=%2FPrim%C3%A4rkodierung%20Datenauswertung%20_%20Primary%20coding%20Data%20analysis](https://cloud.napkon.de/s/kgSLW7gkBExBYfC?path=%2FPrim%C3%A4rkodierung%20Datenauswertung%20_%20Primary%20coding%20Data%20analysis)

Additionally, there are manuals that include variable categorizations, aids for calculating scores, defining normal ranges of laboratory parameters and clinical parameters. Further explanations are also available in the tutorial presentation. 


## Repository structure

This repository is structured as follows:
- [R](R) contains all R code
- [data/import](data/import) is the place to put raw datasets, i.e. secuTrial exports
- [data/export](data/export) is the place where exported datasets are stored
- [doc](doc) contains further documentation
- [lib](lib) contains external libraries
- [resources](resources) contains static content such as images
- [tests](tests) contains unit tests

### :computer: R code structure

The R code is structured as follows:
- [R/*.R](R) contains platform independent implementations:
    - [10-preprocess-zip.R](R/10-preprocess-zip.R) contains a function to preprocess the exported zip
    - [20-import-secutrial.R](R/20-import-secutrial.R) contains secutrial import functions
    - [30-primary-coding.R](R/30-primary-coding.R) contains platformindependent helper fuctions used in platform specific primary coding scripts
    - [40-render.R](R/40-render.R) contains a function for rendering reports
    - [41-render-diagrams.R](R/41-render-diagrams.R) contains a function for rendering diagrams
    - [52-render-helpers.R](R/52-render-helpers.R) contains a workaround to replace NAs in order to be able to generate tables with createTableOne without loosing rows due to all NAs in a data frame column
- [R/hap](R/hap) contains HAP specific implementations
- [R/pop](R/pop) contains POP specific implementations
- [R/suep](R/suep) contains SÜP specific implementations

#### SÜP

Inside [R/suep](R/suep), the code is structured as follows:
- [run](R/suep/run) contains all run scripts, e.g. to code and export to SPSS format 
- [30-primary-coding.R](R/suep/30-primary-coding.R) implements the primary coding of suep
- [XX-export.R](R/suep/XX-export.R) contains export functions, e.g. in SPSS format

#### HAP

Inside [R/hap](R/hap), the code is structured as follows:
- [run](R/hap/run) contains all run scripts, e.g. to code and export to SPSS format 
- [30-primary-coding.R](R/hap/30-primary-coding.R) implements the primary coding of hap
- [XX-export.R](R/hap/XX-export.R) contains export functions, e.g. in SPSS format

#### POP

Inside [R/pop](R/pop), the code is structured as follows:
- [run](R/pop/run) contains all run scripts, e.g. to code and export to SPSS format 
- [30-primary-coding.R](R/pop/30-primary-coding.R) implements the primary coding of hap
- [XX-export.R](R/pop/XX-export.R) contains export functions, e.g. in SPSS format

