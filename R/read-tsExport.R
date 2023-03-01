#' Title: Read SecuTrial Data exported from NAPKON Transferstelle.
#' Version: 1.0 beta
#' Authors@R: 
#'  c(
#'  person("Institut für Medizinische Informatik der Universitätsmedizin Göttingen", 
#'         role=c("cph"),
#'         email = "medinf.transferstelle.napkon@med.uni-goettingen.de"
#'        ),
#'  person("Miriam", "Rainers",
#'         role = c("cre", "aut"),
#'         email = "miriam.rainers@med.uni-goettingen.de"
#'        )
#'   )
#'  
#'  License: MIT; 
#'    YEAR: 2022, 
#'    
#'  Encoding: UTF-8
#'
#' Read SecuTrial Data exported from NAPKON Transferstelle.
#' 
#' Format data similar to the secuTrialR package so that procedures programmed
#' on the basis of the secuTrialdata class will also work with the
#' Transferstellen export data.
#' 
#' Unfortunatly the secuTrialR package failed to read the NAPKON data from the
#' Transferstellen export correctly. 
#' 
#' @author Miriam Rainers <miriam.rainers@med.uni-goettingen.de>,
#' Transferstelle NAPKON <medinf.transferstelle.napkon@med.uni-goettingen.de>
#' 
#' 
#'
# ------------------------------------------------------------------------------
# READ TRANSFERSTELLE EXPORT RAW
# ------------------------------------------------------------------------------
#
#' Load an individual table from a NAPKON transferstelle data export. 
#' 
#' @description
#' This function loads an individual csv file from an zip archive or directory
#' provided by NAPKON Transferstelle export, which basicly is a prepcrocessed
#' Secutrial export.  
#'
#' This is an internal function which is wrapped by read_tsExport_raw
#' 
#' @details In older tsExports the data files that have been preprocessed use
#' '""' (double quotes) as quote escaping, which is the standard option with
#' write_csv2, while the meta files which weren't preprocessed but just copied
#' into the tsExport use '\"' (backslash) as quote escape, which is the standard
#' for (german) Secutrial exports.In more recent tsExports this should be fixed
#' and all data files have backslash escaping. However, to make this work for
#' older exports as well, this function attempts to detect the quote_escaping
#' that's actually present in a file.
#' 
#' Secutrial attaches an empty column at the end of each scv table. In the
#' preprocessing of tha data files, the empty column was removed by NAPKON
#' Transferstelle. However it is still present in the meta files. Therefore this
#' funcion also removes the empty last column if present.
#' 
#' @param data_dir String. Path with directory or zip file of the Transferstelle 
#'   export.
#' @param file_name String. Name of the csv file which should be read.
#' @param separator Character. CSV file column separator.
#' @param decimal Character. 
#' @param quote_escape_detect Boolean.Should the function try to detect 
#'   the escaping method used in the data file? If TRUE, the reading process 
#'   might be slower.
#' @param escape_backslash Boolean.Are Quotes in strings escaped by a
#'   backslash? (The default with (german) secuTrial Exports).
#' @param encoding Character. Encoding
#' 
#' 
#' @return The function returns a tibble for the data in file_name.
#' 
#' @seealso read_tsExport_raw
#' @importFrom utils unzip
#' @import stringr 
#' @import readr 
#' @noRd
#' 
# TODO: copied from readame to delete readme for package comliance, redo?
# # ### Usage of read_tsExport.R
# 
# library(tidyverse)
# 
# SCRIPTFILE <- file.path("/path/to/Script",
#                         "read_tsExport.R")
# 
# source(SCRIPTFILE)
# 
# EXPORT_DIR <- file.path("/path/to/secutrial or transferstelle",
#                         "export_directory")
# 
# ### Step by step reading
# 
# my_export_raw <- read_tsExport_raw(EXPORT_DIR, separator = ";", decimal = ",") # read files without changing data
# my_export_labelled <- label_tsExport(my_export_raw)                            # label variables
# my_export_valluelabelled <- valuelabels_tsExport(my_export_labelled)           # set value labels attribute to variable without creating factors
# \# or
# my_export_factorized <- factorize_tsExport(my_export_labelled)                 # set value labels attribute to variable and create factors 
# my_export_dated <- dates_tsExport(my_export_factorized)                        # convert date variables from numbers/strings to variables of date class
# 
# 
# ### Read with all steps combined
# 
# my_export <- read_tsExport(EXPORT_DIR, separator = ",", decimal = ".")
# 
# \# The options separator and decimal can be ommited if they are defaults (sep. = ";" and dec. = ",")
# 
# 
# ### load data tables into global environment
# 
# my_export %>%
#   keep(names(.) %in% my_export$export_options$studydata_names) %>%
#   list2env(envir = .GlobalEnv)
# TODO: ...redo until here?
read_tsExport_table <- function(data_dir, file_name, 
                                separator = ";", decimal = ",",
                                encoding = "UTF-8",
                                quote_escape_detect = FALSE,
                                escape_backslash = TRUE){
  
  is_zip <- grepl(".zip$", data_dir)
  
  is_dir <- dir.exists(data_dir)
  
  if (is_zip) {
    data_file <- unz(data_dir, file_name)
  } else if (is_dir) {
    data_file <- file.path(data_dir, file_name)
  } else {
    stop(paste("no zip file or directory found:", data_dir))
  }
  
  readrformat <- locale(decimal_mark = decimal, grouping_mark = "", 
                        encoding = encoding)
  
  # autodetect quote escaping (older stExports have mixed quote escaping: 
  # "" in data files and \" in meta files :-(
  
  if (quote_escape_detect) {
    print("Quote Escape Detection: TRUE")
    if(read_lines(data_file) %>% str_detect("\\\\\\\"") %>% any()) {
      escape_backslash = TRUE
      }
    }
    else {
      escape_backslash = FALSE
    }
    print(paste("Escape Backslash:", escape_backslash))
    if (is_zip) data_file <- unz(data_dir, file_name) # reload data_file

  # suppress warnings if filename is cl.csv, because this file is bugged. It has
  # different number of columns in different row, which causes non problematic
  # parsing issues.
  if (file_name == "cl.csv") {
    defaultW <- getOption("warn") 
    options(warn = -1)
  }
  
  .data <- read_delim(data_file, delim = separator, locale = readrformat, 
                      escape_backslash = escape_backslash, 
                      escape_double = !escape_backslash, 
                      guess_max = 10000)

  # undo warning suppression
  if (file_name == "cl.csv") options(warn = defaultW)
  
  # remove last empty column added by secutrial export (if it is there)
  lastcolname <- .data %>% names() %>% last()
  # Only do something if last column is (X|...)<number>
  if (str_detect(lastcolname, "(^\\.\\.\\.|^X)\\d*$")) {
    if (.data[[lastcolname]] %>% is.na %>% all) { # if last column is empty, delete it.
      .data <- .data %>% select(-last_col())
    }
  }
  .data
}


#' Load NAPKON Transferstelle export data from directory or zip file.
#' 
#' @description 
#' Load the full set of meta and data tables. 
#' Generate and store export information in export_options.
#' 
#' @param data_dir String. Path with directory or zip file of the Transferstelle 
#'   export.
#' @param separator Character. CSV file column separator.
#' @param decimal Character. 
#' @param quote_escape_detect Boolean.Should the function try to detect 
#'   the escaping method used in the data file? If TRUE, the reading process 
#'   might be slower.
#' @param escape_backslash Boolean.Are Quotes in strings escaped by a
#'   backslash? (The default with (german) secuTrial Exports).
#' @param encoding Character. Encoding
#'   
#' @return \code{tsExportdata} Object containing a list of export data 
#'   information and data.frames with all the data loaded from the 
#'   Transferstelle export.
#' @importFrom stats setNames  
#' @importFrom rlang .data
#' @export 
read_tsExport_raw <- function(data_dir,
                              separator = ";", decimal = ",",
                              encoding = "UTF-8",
                              quote_escape_detect = TRUE,
                              escape_backslash = TRUE) {
  
  # create some of the export options you will also find in the 
  # secuTrialdata class.
  export_options <- list()
  
  export_options$data_dir <- data_dir
  export_options$sep <- separator
  export_options$encoding <- encoding
  export_options$date_format <- "%Y%m%d"

  # Check if data_dir is an existing zip file or directory.
  is_zip <- grepl(".zip$", data_dir)
  export_options$is_zip <- is_zip
  
  is_dir <- dir.exists(data_dir)
  
  if (is_zip) {
    data_files <- unzip(data_dir, list = TRUE)$Name %>% str_subset(".csv$")
  } else if (is_dir) {
    data_files <- list.files(data_dir, pattern="*.csv")
  } else {
    stop(paste("no zip file or directory found:", data_dir))
  }
  
  # list of data set names: filenames without leading special characters
  data_names <- tools::file_path_sans_ext(data_files) %>%
    str_replace("^(e?)[^a-zA-Z]", "\\1")
  
  ### store file information into export_options
  
  # (incomplete) list of meta files generated by SecuTrial export
  meta_names <- sort(c("cl", "dc", "is", "fs", "qs", "vp", "vpfs", "cn", "ctr"))
  
  export_options$data_files <- data_files
  export_options$data_names <- data_names %>% set_names(data_files)
  export_options$studydata_files <- data_files %>% dplyr::setdiff(str_c(meta_names, ".csv"))
  export_options$studydata_names <- data_names %>% dplyr::setdiff(meta_names) 
  export_options$meta_files <- data_files %>% dplyr::intersect(str_c(meta_names, ".csv"))
  export_options$meta_names <- data_names %>% dplyr::intersect(meta_names)
  
  
  # Read files from NAPKON Transferstelle export
  tsExport <- data_files %>%
    map(~ {
      print(paste("Read:", .x))
      read_tsExport_table(data_dir, .x, separator, decimal, 
                          encoding, quote_escape_detect, escape_backslash)}) %>%
    setNames(data_names)
  

  # retrieve study id from fs.csv formtablename column.
  # For the HAP it may return num2_ if the file names where manipulated by
  # Transferstelle preprocessing, but that should be no problem.
  get_study_id <- function(tsExport) {
    
    if (!("fs" %in% names(tsExport))) {
      stop("No table named \"fs\" in tsExport")
    }
    
    fs <- tsExport$fs
    
    study_id <- fs %>%
      distinct(.data$formtablename) %>%
      pull(.data$formtablename) %>%
      str_subset("mnp") %>%
      str_remove("^e?mnp") %>%
      .[1] %>%
      str_remove(str_c(tools::file_path_sans_ext(data_files), "$", collapse = "|"))
    
    return(study_id)
  }
  
  export_options$study_id <- get_study_id(tsExport)
  
  # If "fs" is available retrieve form order from fs.csv formtablename 
  # column and with the help of study_id, else use alphabetical order of 
  # studydata_names.
  
  get_form_order <- function(fs, study_id) {
    fs %>%
      distinct(.data$formtablename) %>%
      pull(.data$formtablename) %>%
      str_remove(str_c("mnp", study_id, "[^a-zA-Z]?"))
  }
  
  if("fs" %in% names(tsExport)) {
    export_options$form_order <- get_form_order(tsExport$fs, export_options$study_id)
  } else {
    export_options$form_order <- sort(tsExport$export_options$studydata_names)
  }
  
  export_options$labelled <- FALSE
  export_options$factorized <- FALSE
  export_options$dated <- FALSE
  
  # attach export_options to tsExport
  tsExport$export_options <- export_options
  
  # Sort elements in ts-Export:
  # 1. export_options
  # 2. study data tables by form_order
  # 3. meta data tables
  tsExport <- tsExport[order(match(names(tsExport), 
                                   c("export_options",
                                     tsExport$export_options$form_order,
                                     tsExport$export_options$meta_names)))]
  
  
  class(tsExport) <- "tsExportdata"

  return(tsExport)
}

# print function for tsExportdata copied from secuTrialR package version 1.0.9.
print.tsExportdata <- function(x, ...) {
  
  cat("secuTrial data imported from:\n")
  cat(str_wrap(x$export_options$data_dir, width = 80), "\n")
  
  tab <- lapply(x$export_options$data_names, function(y) {
    tmp <- x[[y]]
    tmp
    data.frame(table = y,
               nrow = nrow(tmp),
               ncol = ncol(tmp),
               meta = y %in% x$export_options$meta_names)
  })
  tab <- do.call("rbind", tab)
  tab$original_name <- rownames(tab)
  rownames(tab) <- NULL
  print(tab, row.names = FALSE)
  
}


# ------------------------------------------------------------------------------
# LABEL TRANSFERSTELLE EXPORT VARIABLES
# ------------------------------------------------------------------------------
#' Create a list that stores the variable attributes: label, unit and type
#' 
#' @importFrom rlang enquo
#' @importFrom rlang .data 
#' @importFrom lubridate interval
#' @import dplyr 
#' @import tidyr 
#' 
#' @param is Data.Frame that contains the item information. Usually this is 
#'   named "is".
#' @param study_id String. Study ID in secuTrial.
#' @return named list of study data table names, their variables and the
#'   attributes those variables have.
#' @noRd
create_varlab_lookup <- function(is, study_id) {
  
  lookup_table <- is %>% 
    filter(!is.na(.data$ffcolname)) %>%
    # keep only label information for the current project version
    group_by(.data$formtablename, .data$ffcolname) %>%
    slice_min(.data$fgid) %>%
    ungroup() %>%
    # compute studydata_name from formtablename
    mutate(studydata_name = str_remove(
      .data$formtablename, (str_c("mnp", study_id, "[^a-zA-Z]?")))) %>%
    # remove html tags and square brackets from unit column
    mutate(unit = str_remove_all(.data$unit, "</?[:alpha:]+>|\\[|\\]")) %>%
    select(.data$studydata_name, varname = .data$ffcolname, label = .data$fflabel, .data$unit, 
           type = .data$itemtype)
  
  lookup_list <- lookup_table %>%
    mutate(attr = pmap(list(label = .data$label, unit = .data$unit, type = .data$type), 
                       list)) %>%
    select(.data$studydata_name, .data$varname, .data$attr) %>%
    group_by(.data$studydata_name) %>%
    nest() %>%
    deframe() %>%
    map(~ {.x %>% 
        group_by(.data$varname) %>%
        nest() %>%
        deframe()}) %>%
    map(~ .x %>%
          map(~ {flatten(flatten(.x))}))
  
  return(lookup_list)
}


#' Set Variable labels in a table
#' 
#' @description 
#' Set the attributes: label, unit and type for variables in a tsExport 
#' data table.
#' 
#' @details 
#' If a variable is found in the lookup table, then this variable will changed
#' to labelled class from the haven package. Other attributes found in the 
#' lookup table such as type and unit will be set to the variable.
#' 
#' However, the attributes will not be propagated if variables are derived and 
#' may be lost if variables are edited. 
#' 
#' @param .data Data.Frame that should be labelled. 
#' @param studydata_name String. Table name in lookup that relates to the table
#'   that get's labelled.
#' @param lookup List that holds lookup information. It is produced by
#'   \code{create_varlab_lookup}
#'   
#' @return Data.Frame with labelled variables
#' 
#' @importFrom rlang .data
#' @noRd
label_tsExport_table <- function(.data, studydata_name, lookup) {
  
  vars_to_label <- names(lookup[[studydata_name]])
  
  .data %>%
    # if variable is NA for every case, convert it to numeric
    mutate(across(where(~ all(is.na(.))), as.numeric)) %>%
    mutate(across((any_of(vars_to_label) & where(~ is.numeric(.) | is.character(.))), 
                  haven::labelled)) %>%
    mutate(across(any_of(vars_to_label), ~ {
      `attributes<-`(., c(attributes(.), 
                          lookup[[studydata_name]][[cur_column()]]))})) 
}


#' Set variable labels in NAPKON Transferstelle Export study data tables
#' 
#' @description 
#' Set the variable labels and other variable attributes such as unit and 
#' variable type, for all variables found in the is.csv file.
#' 
#' Add variable lookup list to export_options
#' Mark the tsExport object as labelled in export_options
#' 
#' @param tsExport \code{tsExportdata} that should be labelled
#' 
#' @return labelled \code{tsExportdata}
#' 
#' @seealso label_tsExport_table
#' @importFrom rlang .data
#' @export
label_tsExport <- function(tsExport) {
  
  # check if is table is in the data export
  if (!("is" %in% names(tsExport))) {
    stop("Can't label variables. No table named \"is\" in tsExport")
  }
  
  # generate lookup list for variable attributes
  varlab_lookup <- create_varlab_lookup(
    tsExport$is, tsExport$export_options$study_id)
  
  # Clean varlab_lookup from form entries, that are not in form_names &
  # sort it by formorder
  xx <- varlab_lookup %>%
    keep(names(.) %in% tsExport$export_options$studydata_names) %>%
    .[order(match(names(.), tsExport$export_options$form_order))]


  tsExport$export_options$varlab_lookup <- varlab_lookup
  
  # label study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(varlab_lookup)) {
      .x %>%
        label_tsExport_table(.y, varlab_lookup)
    } else {
      .x
    })
  
  tsExport$export_options$labelled <- TRUE
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}


# ------------------------------------------------------------------------------
# FACTORIZE TRANSFERSTELLE EXPORT VARIABLES
# ------------------------------------------------------------------------------

#' Create a list that stores the value labels
#' 
#' @import stringr 
#' @importFrom rlang .data
#' @param cl Data.Frame that contains the value label information. Usually this
#'  is "cl".
#' @param study_id String. Study ID in secuTrial.
#' @return named list of study data table names, their variables and the
#'   value codes and labels those variables have.
#' @noRd
create_vallab_lookup <- function(cl, study_id) {
  
  # suppress summarise inform message
  default_summarize_inform <- getOption("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)
  
  lookup_table <- cl %>%
    # delete meta var labels
    filter(str_detect(.data$column, str_c("mnp", study_id))) %>% 
    # extract form names
    mutate(studydata_name = str_extract(.data$column, "^[^\\.]*") %>%
             str_remove(str_c("mnp", study_id, "[^a-zA-Z]?")),
           varname = str_extract(.data$column, "[^\\.]*$")) %>% 
    # order values (negative values at the end)
    mutate(positive = .data$code >= 0,
           abscode = abs(.data$code)) %>%
    arrange(.data$studydata_name, .data$varname, desc(.data$positive), .data$abscode) %>%
    group_by(.data$studydata_name, .data$varname) %>% 
    summarise(values = list(.data$code),
              labels = list(.data$value)) %>%
    ungroup()
  
  # restore summarise inform message option
  options(dplyr.summarise.inform = default_summarize_inform)
  
  lookup_list <- lookup_table %>%
    mutate(vallabs = pmap(list(values = .data$values, labels = .data$labels), 
                          list)) %>%
    select(.data$studydata_name, .data$varname, .data$vallabs) %>%
    group_by(.data$studydata_name) %>%
    nest() %>%
    deframe() %>%
    map(~ {.x %>% 
        group_by(.data$varname) %>%
        nest() %>%
        deframe()}) %>%
    map(~ .x %>%
          map(~ {flatten(flatten(.x))}))
  
  return(lookup_list)
}

#' Set Value labels in a table
#' 
#' @description 
#' Set the attribute: labels in a tsExport data table.
#' 
#' @details 
#' If a variable is found in the lookup table, and it is already a labelled 
#' class vector, then just attach the labels attribute with the labels from the
#' lookup list.
#' If the variable is not a labelled class convert it with the labelled function
#' and attach value label function.
#' 
#' @param .data Data.Frame that should be labelled. 
#' @param studydata_name String. Table name in lookup that relates to the table
#'   that get's labelled.
#' @param lookup List that holds lookup information. It is produced by
#'   \code{create_vallab_lookup}
#'   
#' @return Data.Frame with (value) labelled variables
#' @import dplyr
#' @noRd
valuelabels_tsExport_table <- function(.data, studydata_name, lookup) {
  
  labelled_attrsave <- function(x, labels = NULL, label = NULL) {
    ### convert a vector to a labelled vector, but keep the already existing 
    ### attributes.
    
    # store attributes
    x_attr <- attributes(x)
    
    # replace label and labels attributes if they are not NULL
    if(!is.null(labels)) x_attr$labels <- labels
    if(!is.null(label)) x_attr$label <- label
    
    # make x labelled and restore the attributes
    x <- haven::labelled(x)
    attributes(x) <- c(attributes(x), x_attr)
    
    return(x)
  } 
  
  vars_to_label <- names(lookup[[studydata_name]])
  
  .data %>%
    # if variable is NA for every case, convert it to numeric
    mutate(across(where(~ (is.logical(.) & all(is.na(.)))), as.numeric)) %>%
    # convert to haven::labelled and attach labels attribute 
    mutate(across(
      (any_of(vars_to_label) & where(where(~ is.numeric(.) | is.character(.)))),
      ~ {labelled_attrsave(
        .x, labels = (lookup[[studydata_name]][[cur_column()]]$values %>%
                        set_names(lookup[[studydata_name]][[cur_column()]]$labels)))}
    ))
  
}


#' Set value labels in NAPKON Transferstelle Export study data tables
#' 
#' @description 
#' Set value labels for all variables found in the cl.csv file.
#' 
#' Add value_labels lookup list to export_options.
#' 
#' @import stringr 
#' @import dplyr
#' @import tidyr
#' @importFrom rlang .data
#' 
#' @param tsExport \code{tsExportdata} that should get value labels
#' 
#' @return \code{tsExportdata} with value labels
#' 
#' @seealso valuelabels_tsExport_table
#' @noRd
valuelabels_tsExport <- function(tsExport) {
  
  # check if cl table is in the data export
  if (!("cl" %in% names(tsExport))) {
    stop("Can't create value labels. No table named \"cl\" in tsExport")
  }
  
  # generate lookup list for value labels
  vallab_lookup <- create_vallab_lookup(
    tsExport$cl, tsExport$export_options$study_id)
  
  # Clean vallab_lookup from form entries, that are not in form_names &
  # sort it by formorder
  vallab_lookup <- vallab_lookup %>%
    keep(names(.) %in% tsExport$export_options$studydata_names) %>%
    .[order(match(names(.), tsExport$export_options$form_order))]
  
  tsExport$export_options$vallab_lookup <- vallab_lookup
  
  # label study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(vallab_lookup)) {
      .x %>%
        valuelabels_tsExport_table(.y, vallab_lookup)
    } else {
      .x
    })
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}



#' factorize table
#' 
#' @description 
#' For variables in a tsExport data table that don't have value labels call 
#' /code{valuelabels_tsExport_table}.
#' For each variable with value labels in the data table create a factored 
#' variant of this variable, which has the postfix ".factor", and copy the 
#' variable attributes.
#' 
#' @param .data Data.Frame that should be factorized. 
#' @param studydata_name String. Table name in lookup that relates to the table
#'   that get's labelled.
#' @param lookup List that holds lookup information. It is produced by
#'   \code{create_vallab_lookup}
#'   
#' @return Data.Frame with (value) labelled variables and factorized variants of
#'   these variables.
#' @import dplyr
#' @importFrom rlang .data
#' @noRd
factorize_tsExport_table <- function(.data, studydata_name, lookup) {

  # check if an object has a certain attribute
  has_attribute <- function(x, attr) {
    attr %in% names(attributes(x))
  }
  
  # check if table is already labeled. If not, do so.
  is.labelled.data.frame <- function(.data) {
    .data %>%
      map(~ {haven::is.labelled(.x) & has_attribute(.x, "labels")}) %>%
      unlist() %>%
      any()
  }
  
  if(!is.labelled.data.frame(.data)) {
    #  print("Make it so!")
    .data <- valuelabels_tsExport_table(.data, studydata_name, lookup)
  }
  
  # convert a labelled vector to factor, but keep the already existing 
  # attributes.
  as_factor_attrsave <- function(x) {
    x_attr <- attributes(x)
    x_attr$class <- NULL
    
    # make x labelled and restore the attributes
    x <- haven::as_factor(x)
    attributes(x) <- c(attributes(x), x_attr)
    
    return(x)
  }
  
  labelled_vars <- .data %>%
    select(where(~ has_attribute(., "labels"))) %>%
    colnames()
  
  # Add factorized variables to table
  .data <- .data %>% 
    mutate(across(all_of(labelled_vars), 
                  as_factor_attrsave, 
                  .names = "{.col}.factor")) %>%
    reduce2(
      .x = labelled_vars,
      .y = str_c(labelled_vars, ".factor"),
      .f = ~ relocate(., {..3}, .after = {..2}),
      .init = .
    )
  
   # Remove value labels from the original variable, so the numeric values are kept as is when exporting to e.g. spss
  .data <- .data %>% 
    mutate(across(where(is.labelled) & !ends_with(".factor"), 
                  haven::zap_labels)) 
  
  return(.data)
}


#' Factorize in NAPKON Transferstelle Export study data tables
#' 
#' @description 
#' For variables with labels found in the in the cl.csv file.
#' 1. set value labels if not already labelled
#' 2. create factor variables from those variables
#' 
#' Add value_labels lookup list to export_options, if not already present.
#' Mark the tsExport object as factorized in export_options.
#' 
#' @import dplyr
#' @importFrom rlang .data
#' @param tsExport \code{tsExportdata} that should get factorized.
#' 
#' @return \code{tsExportdata} with value labels
#' 
#' @seealso factorize_tsExport_table
#' @export
factorize_tsExport <- function(tsExport) {
  
  # check if cl table is in the data export
  if (!("cl" %in% names(tsExport))) {
    stop("Can't factorize data. No table named \"cl\" in tsExport")
  }
  
  # generate lookup list for value labels if not present in export_options.
  if(is.null(tsExport$export_options$vallab_lookup)) {
    vallab_lookup <- create_vallab_lookup(
      tsExport$cl, tsExport$export_options$study_id)
    
    # Clean vallab_lookup from form entries, that are not in form_names &
    # sort it by formorder
    vallab_lookup <- vallab_lookup %>%
      keep(names(.) %in% tsExport$export_options$studydata_names) %>%
      .[order(match(names(.), tsExport$export_options$form_order))]
    
    tsExport$export_options$vallab_lookup <- vallab_lookup
  } else {
    vallab_lookup <- tsExport$export_options$vallab_lookup
  }
  
  # factorize study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(vallab_lookup)) {
      # print(paste("Factorize:", .y))
      .x %>%
        factorize_tsExport_table(.y, vallab_lookup)
    } else {
      .x
    })
  
  tsExport$export_options$factorized <- TRUE
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}


# ------------------------------------------------------------------------------
# TRANSFORM DATES TRANSFERSTELLE EXPORT VARIABLES
# ------------------------------------------------------------------------------

#' Create a list that stores the date variable names per study data table
#' 
#' @import stringr 
#' @import dplyr
#' @import tibble
#' @import purrr  
#' @importFrom rlang .data    
#' @param is Data.Frame that contains the item information. Usually this is 
#'   named "is".
#' @param study_id String. Study ID in secuTrial.
#' @return named list of study data table names and their date variables
#' @noRd
create_date_lookup <- function(is, study_id) {
  
  lookup_table <- is %>% 
    # keep only label information for the current project version
    group_by(.data$formtablename, .data$ffcolname) %>%
    slice_min(.data$fgid) %>%
    ungroup() %>%
    filter(str_detect(.data$itemtype, "Datum")) %>% 
    # compute studydata_name from formtablename
    mutate(studydata_name = str_remove(
      .data$formtablename, str_c("mnp", study_id, "[^a-zA-Z]?"))) %>%
    # remove html tags and square brackets from unit column
    mutate(unit = str_remove_all(.data$unit, "</?[:alpha:]+>|\\[|\\]")) %>%
    select(.data$studydata_name, varname = .data$ffcolname, type = .data$itemtype)
  
  lookup_list <- lookup_table %>%
    select(.data$studydata_name, .data$varname, .data$type) %>%
    group_by(.data$studydata_name) %>%
    nest() %>%
    deframe() %>%
    map(~ {.x %>% 
        group_by(.data$varname) %>%
        nest() %>%
        deframe()}) %>%
    map(~ .x %>%
          map(~ {flatten(flatten(.x))}))
  
  return(lookup_list)
}


#' Convert date variables in table to `Date` class
#' 
#' @description 
#' For date variables found in a tsExport data table, create new variables that 
#' transform the String variable into a `Date` class variable.
#' 
#' @details 
#' The new Date class variables have the postfix ".date" and are placed behing
#' the original secuTrial variable.
#' Copy variable label to dated Variable
#' 
#' @param .data Data.Frame that should get formatted dates. 
#' @param studydata_name String. Table name in lookup that relates to the table
#'   that get's labelled.
#' @param lookup List that holds date variables lookup information. It is 
#'   produced by \code{create_datevarnames_list}
#'
#' @return Data.Frame with converted date variables.
#' @noRd
dates_tsExport_table <- function(.data, studydata_name, lookup) {
  
  # convert a date variable to date class and keep label attribute.
  as_Date_labelled <- function(x) {
    
    label <- attr(x, "label")
    
    x <- as.Date(as.character(x), format = "%Y%m%d")
    x <- `attr<-`(x, "label", label)
    return(x)
  }
  
  date_vars <- names(lookup[[studydata_name]]) %>% 
    dplyr::intersect(names(.data))
  
  # compute and add converted date variable to table.
  .data <- .data %>% 
    mutate(across(all_of(date_vars), 
                  as_Date_labelled, 
                  .names = "{.col}.date")) %>%
    reduce2(
      .x = date_vars,
      .y = str_c(date_vars, ".date"),
      .f = ~ relocate(., {..3}, .after = {..2}),
      .init = .
    )
  
  return(.data)
}


#' Convert dates in NAPKON Transferstelle Export study data tables
#' @description 
#' Convert date variables from string class to Date class.
#' Keep the original Variables and attach the postfix ".date" to the convertet
#' variables.
#' 
#' Add date lookup list to export_options
#' Mark the tsExport object as dated in export_options
#' 
#' @param tsExport \code{tsExportdata} that should be dated
#' 
#' @return dated \code{tsExportdata}
#' 
#' @seealso dates_tsExport_table
#' @importFrom rlang .data
#' @export
dates_tsExport <- function(tsExport) {
  
  # check if is table is in the data export
  if (!("is" %in% names(tsExport))) {
    stop("Can't label variables. No table named \"is\" in tsExport")
  }
  
  # generate lookup list for variable attributes
  date_lookup <- create_date_lookup(
    tsExport$is, tsExport$export_options$study_id)
  
  # Clean date_lookup from form entries, that are not in form_names &
  # sort it by formorder
  date_lookup <- date_lookup %>%
    keep(names(.) %in% tsExport$export_options$studydata_names) %>%
    .[order(match(names(.), tsExport$export_options$form_order))]
  
  tsExport$export_options$date_lookup <- date_lookup
  
  # convert dates in study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(date_lookup)) {
      .x %>%
        dates_tsExport_table(.y, date_lookup)
    } else {
      .x
    })
  
  tsExport$export_options$dated <- TRUE
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}


# ------------------------------------------------------------------------------
# READ TSEXPORT WRAPPER FUNCTION
# ------------------------------------------------------------------------------

#' Read NAPKON Transferstelle export
#' @description 
#' Convenience wrapper for \code{read_tsExport_raw}, ' \code{label_tsExport},
#' \code{factorize_tsExport} and \code{dates_tsExport}.
#' @param data_dir string - location of the export
#' @param labels logical - add labels to variables and table
#' @param factor logical - convert categorical variables to factor variables
#'               (ignored when reference values are not in a separate table)
#' @param dates  logical - convert date variables
#' @param separator Character. CSV file column separator.
#' @param decimal Character. 
#' @param quote_escape_detect Boolean.Should the function try to detect 
#'   the escaping method used in the data file? If TRUE, the reading process 
#'   might be slower.
#' @param escape_backslash Boolean.Are Quotes in strings escaped by a
#'   backslash? (The default with (german) secuTrial Exports).
#' @param encoding Character. Encoding
#'
#' @return \code{tsExportdata} object - a list with one data.frame for each file 
#'   on the export and a list containing the export options
#' @export
read_tsExport <- function(data_dir, separator = ";", decimal = ",",
                          encoding = "UTF-8",
                          quote_escape_detect = TRUE,
                          escape_backslash = TRUE,
                          labels = TRUE, factor = TRUE, dates = TRUE) {
  
  # check for file existence
  if (! file.exists(data_dir)) {
    stop(paste0("There is no file '", data_dir, "'"))
  }
  
  # read raw export
  tryCatch(
    expr = {
      tsExport <- read_tsExport_raw(data_dir, separator, decimal, encoding,
                                    quote_escape_detect, escape_backslash)
      message("Read export successfully.")
    },
    error = function(e) {
      message("Something went wrong, when trying to read tsExport data:")
      message(str_c("\t", as.character(e)))
    }
  )
  # label
  tryCatch(
    expr = {
      if (labels) {
        tsExport <- label_tsExport(tsExport)
        message("Done Labelling.")
      }
    },
    error = function(e) {
      message("label_tsExport() failed. Proceeding without labelling.")
    }
  )
  # factorize
  tryCatch(
    expr = {
      if (factor) {
        tsExport <- factorize_tsExport(tsExport)
        message("Done Factorization.")
      }
    },
    error = function(e) {
      message("factorize_tsExport() failed. Proceeding without factorization.")
    }
  )
  # dates
  tryCatch(
    expr = {
      if (dates) {
        tsExport <- dates_tsExport(tsExport)
        message("Done Date conversion.")
      }
    },
    error = function(e) {
      message("dates_tsExport() failed. Proceeding without date converting.")
    }
  )
  # return output
  tryCatch(
    expr = {
      return(tsExport)
    },
    error = function(e) {
      message("read_tsExport() failed.")
    }
  )
}

