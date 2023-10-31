# Functions to prepare the data for a cleaner SPSS export.
# This includes:
# - Remove factor variables that where generated during the import process,
#   since they are basically duplicates
# - Convert the original nominal/ordinal variables into spss_labelled class and
#   (re)label the values with values from the value-lookup-list created in the 
#   import step.
# - Optionally add Missing Value information for variable values that are 
#   generally considered as missing.
# - Optionally remove original Date-String Variables, if a equivalent date 
#   variable exists.


### Remove Date String variables -----------------------------------------------

#' Remove original secutrial date string variables from data table.
#' 
#' @description Remove original secutrial date string variables, if an equivalent
#' to date converted variable exists. Remove the `.date` suffix from converted 
#' variable.
#' 
#' @param .data Data.Frame where the original secutrial date variables should be 
#' removed. 
#' @param studydata_name String. Table name in lookup that relates to the table.
#' @param lookup List that holds date variables lookup information. It is 
#' produced by [create_date_lookup()]
#'   
#' @return Data.Frame deleted secutrial date variables found in the vallab lookup 
#' list. 
#' 
#' @keywords internal
#' 
remove_st_dates_tsExport_table <- function(.data, studydata_name, date_lookup) {
  
  datevars <- date_lookup[[studydata_name]] %>% names()
  
  # check which date vars have a corresponding .date var. 
  deletevars <- .data %>% 
    names() %>%
    intersect(str_c(datevars, ".date")) %>%
    str_remove("\\.date$")
  
  # delete original secutrial date variables and remove ".date" suffix from the 
  # converted date variables.
  .data <- .data %>%
    select(!any_of(deletevars)) %>%
    rename_with(~ str_remove(., ".date$"), any_of(str_c(deletevars, ".date")))
  
  return(.data)
}


#' Remove original secutrial date string variables from tsExport data.
#' 
#' @description Remove original secutrial date string variables, if an equivalent
#' date type converted variable exists. Remove the `.date` suffix from converted 
#' variable.
#' 
#' @param tsExport \code{tsExportdata} where original date variables should 
#' removed. 
#' @param keep_vars character vector of variable names, that should be kept.
#' 
#' @return \code{tsExportdata} without original secutrial date string variables.
#' 
#' @seealso [remove_st_dates_tsExport_table()]
#' 
#' @export
#' 
remove_st_dates_tsExport <- function(tsExport, keep_vars = NULL){
  
  # check if tsExport has converted date variables. If not return original object.
  if (!tsExport$export_options$dated) {
    print(paste("date variable in stExport have not been converted to date class. ",
                "Keeping the original secutrial variables."))
    
    return(tsExport)
  }
  
  # define date remove lookup table as date_lookup minus original date string 
  # variable names that should be kept.
  date_lookup_rm <- tsExport$export_options$date_lookup %>%
    map(~ .x %>% discard(names(.) %in% c(keep_vars)))
  
  # remove original secutrial date variables from study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(date_lookup_rm)) {
      .x %>%
        remove_st_dates_tsExport_table(.y, date_lookup_rm)
    } else {
      .x
    })
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}   


# ------------------------------------------------------------------------------
# REMOVE FACTORS
# ------------------------------------------------------------------------------

#' Remove factorized variables from tsExport data table.
#' 
#' @description Remove factor variables that where created during the import
#' process from data.
#' 
#' @param .data Data.Frame where the factorized variables should be removed. 
#' @param studydata_name String. Table name in lookup that relates to the table
#'   that get's labelled.
#' @param vallab_lookup List that holds variable label lookup information. It is 
#' produced by [create_vallab_lookup()].
#'   
#' @return Data.Frame without factor variables found in the vallab lookup list. 
#' 
#' @keywords internal
#' 
defactorize_tsExport_table <- function(.data, studydata_name, vallab_lookup){
  
  # delete all ".factor" variables whose names are in the value label lookup table. 
  .data %>%
    select(!any_of(str_c(names(vallab_lookup[[studydata_name]]),
                       ".factor")))
}


#' Remove factorized variables tsExport data.
#' 
#' @description Remove factor variables that where created during the import
#' process from data, since they are basically duplicates of the original 
#' numeric categorical variables and not needed when working with SPSS.
#' 
#' @details 
#' For variables found in the vallab lookup list remove their corresponding 
#' factorized variables.
#' Mark the tsExport object as factorized = FALSE in export_options.
#' 
#' @param tsExport \code{tsExportdata} where factorized variables should removed. 
#' @return \code{tsExportdata} without factor variables found in the vallab 
#' lookup list. 
#' 
#' @export
#' 
defactorize_tsExport <- function(tsExport) {
  
  vallab_lookup <- tsExport$export_options$vallab_lookup
  
  # defactorize study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(vallab_lookup)) {
      .x %>%
        defactorize_tsExport_table(.y, vallab_lookup)
    } else {
      .x
    })
  
  tsExport$export_options$factorized <- FALSE
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}   


# ------------------------------------------------------------------------------
# ADD VALUE LABELS FOR SPSS
# ------------------------------------------------------------------------------

#' Add value labels to tsExport data table.
#' 
#' @description Convert variables found in vallab lookup list into 
#' "haven_labelled_spss" class and set their `labels` attribute according to 
#' their entry in the lookup list. 
#' 
#' @details This functions checks if all data frame variables found in the 
#' varlab lookup list are of haven_labelled class. If so, assume that they are 
#' already correctly labelled. If not apply [valuelabels_tsExport_table()] to 
#' label them. Afterwards add the `haven_labelled_spss` class attribute to those 
#' variables.
#' 
#' @param .data Data.Frame that should be spss_labelled. 
#' @param studydata_name String. Table name in lookup that relates to the table
#'   that get's spss_labelled.
#' @param lookup List that holds lookup information. It is produced by
#'   [create_vallab_lookup()]
#'   
#' @return Data.Frame with value labelled variables
#' 
#' @seealso [valuelabels_tsExport_table()]
#' 
#' @keywords internal
#' 
spss_labelled_tsExport_table <- function(.data, studydata_name, vallab_lookup) {

  # variable names that should be of class haven_labelled
  vars_in_vallab_lookup <- names(vallab_lookup[[studydata_name]])

  # check if all variables are already of class haven_labelled
  all_labelled <- .data %>%
    select(any_of(vars_in_vallab_lookup)) %>%
    select(!where(haven::is.labelled)) %>%
    rlang::is_empty()
  
  # label variables in table if they aren't
  if(!all_labelled) {
    .data <- valuelabels_tsExport_table(.data, studydata_name, vallab_lookup)
  }
  
  # add class attribute "haven_labelled_spss" to variables in lookup list.
  .data <- .data %>%
    mutate(across(any_of(vars_in_vallab_lookup), 
                  ~ `class<-`(., union("haven_labelled_spss", oldClass(.)))))
  
  return(.data)
}


#' Add Value labels to tsExport data.
#' 
#' @description Convert numeric variables in the tsExport data that where found 
#' in the value labels lookup list into haven_labelled_spss class and
#' add value label attributes found in the lookup.
#' 
#' @param tsExport \code{tsExportdata}.
#' 
#' @return \code{tsExportdata} with variables in the vallab lookup list converted
#' to haven_labelled_spss class and their value labels assigned to them.
#' 
#' @seealso [valuelabels_tsExport()] 
#'
#' @export
#' 
spss_labelled_tsExport <- function(tsExport) {
  
  vallab_lookup <- tsExport$export_options$vallab_lookup
  
  # convert variables to haven_labelled_spss in study data tables
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(vallab_lookup)) {
      .x %>%
        spss_labelled_tsExport_table(.y, vallab_lookup)
    } else {
      .x
    })
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}


# ------------------------------------------------------------------------------
# DEFINE MISSING VALUES
# ------------------------------------------------------------------------------

#' Create lookup list for missing values.
#' 
#' @description Create a missing value lookup list by filtering the vallab lookup
#' list for certain values or value labels that are generally considered as 
#' missing.
#' 
#' @details if missing_labels and missing_values are provided to the function,
#' the missing value lookup list contains entries where either the values matching 
#' the missing values or the labels matching the missing labels.
#' 
#' @param lookup vallab lookup list.
#' @param missing_labels character vector of value labels that are generally 
#' considered as missing values.
#' @param missing_values numerical vector of values that are generally considered
#' as missing values.
#' 
#' @return nested list.
#' 
#' @keywords internal
#' 
create_missing_lookup <- function(vallab_lookup, 
                                  missing_labels = NULL,
                                  missing_values = NULL) {
  
  # combine values and labels in the lookup list to named vectors.
  vallab_lookup_zipped <- vallab_lookup %>%
    map_depth(2, ~ `names<-`(.$values, .$labels))
  
  missing_lookup_zipped <- vallab_lookup_zipped %>%
    map(~ .x %>% 
          map(~ .x %>% 
                keep(tolower(names(.)) %in% tolower(missing_labels) |
                       identity(.) %in% missing_values)
          ) %>% compact()
    ) %>% compact()
  
  missing_lookup <- missing_lookup_zipped %>%
    map_depth(2, ~ list(values = unname(.x), labels = names(.)))
  
  # check if a variable has more than 3 missing values. If so print a warning.
  to_many_missing_values <- missing_lookup_zipped %>% 
    map(~ .x %>%
          keep(~ length(.) > 3)) %>%
    compact()
  
  if (!is_empty(to_many_missing_values)) {
    warning("The following variables have more than 3 missing values defined. 
    That might cause an error when exporting to SPSS. \n", 
            to_many_missing_values %>% 
              map(names) %>%
              imap_chr(~ (str_c(.y, ": ", str_c(.x, collapse = ", ")))) %>%
              str_c(collapse = "\n"))
  }
  
  return(missing_lookup)
}


#' Set missing values to variables in a tsExport data table
#' 
#' @description  Add missing value attribute to variables found in the missing 
#' values lookup list.
#' 
#' @details
#' If a variable from the data frame is found in the missing value lookup list,
#' change the variable class to 'haven_labelled_spss' and set the `na_values` 
#' attribute to the values stored in the lookup.
#' 
#' @param .data Data.Frame that should get missings attributed. 
#' @param studydata_name String. Table name in lookup that relates to the table.
#' @param missing_lookup List that holds missing values lookup information. 
#' It is produced by [create_missing_lookup()].
#'   
#' @return Data.Frame with `na_values` attributed haven_labelled_spss class 
#' variables.
#' 
#' @keywords internal
#' 
spss_missings_tsExport_table <- function(.data, studydata_name, missing_lookup){
  
  missings_attrsave <- function(x, na_values = NULL) {
    
    ### convert a vector to a spss labelled vector, but keep the already existing 
    ### attributes.
    
    # store attributes without old missing attributes
    x_attr <- attributes(x) %>%
      purrr::discard(names(.) %in% c("na_values", "class"))
    
    # make x labelled and restore the attributes
    x <- haven::labelled_spss(x, na_values = na_values)
    
    attributes(x) <- c(attributes(x), x_attr)
    
    return(x)
  }
  
  vars_in_missing_lookup <- names(missing_lookup[[studydata_name]])
  
  # set missing value attribute (this overwrites existing ones)
  .data <- .data %>%
    mutate(across(any_of(vars_in_missing_lookup), 
                  ~ missings_attrsave(
                    .x,
                    missing_lookup[[studydata_name]][[cur_column()]]$values)))
  
  return(.data)
}


#' Set Missing Values in tsExport.
#' 
#' @description Set missing value attribute according to the missing value
#' lookup list. If no lookup list exist in tha tsExport object or 
#' overwrite_lookup is TRUE create a new lookup list for for variables whose 
#' values match certain labels or numeric values that are generally 
#' considered as missing.
#' 
#' @details When a new missing value lookup list is created, it uses the value
#' label lookup list as foundation. The value lookup list get's filtered for 
#' matching labels or values that are provided as arguments to the function.  
#' As default, \code{missing_labels_default} is used to create the lookup list.
#' Afterwards variables found in the missing lookuplist, are converted to 
#' 'haven_labelled_spss' class which uses the the `na_values` attribute to define
#' missing values. This attribute is set for each variable found in the 
#' missing value lookup list.
#' 
#' @param tsExport \code{tsExportdata}. 
#' @param missing_labels character vector of value labels that are generally 
#' considered as missing values.
#' @param missing_values numerical vector of values that are generally considered
#' as missing values.
#' @param overwrite_lookup Boolean Should a new missing value lookup list created
#' when the tsExportdata object already contains one?
#' 
#' @return \code{tsExportdata} with newly created missing value lookup list in
#' export_options and  `na_values` attributed variables.
#' 
#' @seealso [create_missing_lookup()] [spss_missings_tsExport_table()]
#' 
#' @export
#' 
spss_missings_tsExport <- function(tsExport, 
                                   missing_labels = missing_labels_default,
                                   missing_values = NULL,
                                   overwrite_lookup = TRUE) {
  
  # if overwrite_lookup = TRUE or missing lookup does not exist in stData
  # create missing values lookup list and store it in the stExport data object.
  if(overwrite_lookup || is.null(tsExport$export_options$missing_lookup)) {
    missing_lookup <- create_missing_lookup(
      tsExport$export_options$vallab_lookup,
      missing_labels = missing_labels,
      missing_values = missing_values)
    
    tsExport$export_options$missing_lookup <- missing_lookup
  } else {
    missing_lookup <- tsExport$export_options$missing_lookup
  }
  
  # set missing values attributes in all data tables for all variables found
  # in missing lookup.
  tsExport <- tsExport %>%
    imap(~ if (.y %in% names(missing_lookup)) {
      .x %>%
        spss_missings_tsExport_table(.y, missing_lookup)
    } else {
      .x
    })

  class(tsExport) <- "tsExportdata"
  return(tsExport)
}


# ------------------------------------------------------------------------------
# PREPARE SPSS EXPORT WRAPPER FUNCTION
# ------------------------------------------------------------------------------

#' Prepare SPSS Export
#' 
#' @description modify tsExportdata for later export to SPSS files with haven
#' package used by the write_tsExport function.
#' 
#' @details The following transformations will be made:
#' - assign value labels for variables found in the vallab lookup.
#' - remove factor variables if they have an equivalent numeric variable that
#'   is found in the vallab lookup.
#' - optional assign general missing values. 
#' - optional remove original secutrial date string variables that have an 
#'    equivalent variable converted to date format.
#'   
#' @param tsExport \code{tsExportdata}. 
#' @param rm_stdates Boolean. If TRUE, original secutrial string date variables
#' will be deleted, if it has an equivalent date converted variable.  
#' @param set_missings Boolean. If TRUE, generic missing values will be assigned 
#' to the data. 
#' @param ... Additional arguments passed to `prepare_spss_export`.
#' - \code{keep_date_vars} defines which original date variables should be kept
#' when rm_stdates is set to TRUE. Default: "birthdate_score", because it not 
#' really a date and will not represented correctly in SPSS. 
#' - \code{missing_values} and \code{missing_labels} define variable values and/or
#' value labels that should be defined as missing values. 
#' Default: `missing_labels = missing_labels_default`
#' - \code{overwrite_missing_lookup} Boolean Should a new missing value lookup 
#' list created if the tsExportdata object already contains one? Default: TRUE
#' 
#' @seealso [defactorize_tsExport()] [spss_labelled_tsExport()] 
#' [remove_st_dates_tsExport()] [spss_missings_tsExport()]
#' 
#' @md 
#' @export
#' 
prepare_spss_export <- function(tsExport, 
                                rm_stdates = TRUE,
                                set_missings = TRUE,
                                ...){
  args <- rlang::list2(...)
  
  # remove factor variables and set value labels
  message("Removing factor variables and setting value labels.")
  
  tsExport <- tsExport %>% 
    defactorize_tsExport %>%
    spss_labelled_tsExport
  
  
  # remove secutrial string date variables
  if (rm_stdates) {
    message("Removing string date variables.")
    
    if ("keep_vars" %in% names(args))
      tsExport <- tsExport %>% 
        remove_st_dates_tsExport(keep_vars = args$keep_date_vars)
    else 
      tsExport <- tsExport %>%
        remove_st_dates_tsExport(keep_vars = "birthdate_score")
  }
  
  
  # set missing values
  if(set_missings) {
    message("Setting missing values.")
    
    if (is.null(args$overwrite_missing_lookup)) 
      args$overwrite_missing_lookup <- TRUE
    
    if (any(c("missing_labels", "missing_values") %in% names(args))) {
      if ("missing_labels" %in% names(args)) missing_labels <- args$missing_labels
      else missing_labels <- missing_labels_default
      tsExport <- tsExport %>% 
        spss_missings_tsExport(missing_labels = missing_labels,
                               missing_values = args$missing_values,
                               overwrite_lookup = args$overwrite_missing_lookup)
    }
    else 
      tsExport <- tsExport %>% spss_missings_tsExport()
  }
  
  
  class(tsExport) <- "tsExportdata"
  return(tsExport)
}


  
