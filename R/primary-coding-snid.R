#################
# Primary coding for SNID secuTrial data via secuTrialR ================
#
# 
#
#################

#' Print current call stack
#'
#' @param ... Previous message to append to
#' @param callstack Message that will be appended
#' @importFrom utils head
#' @noRd

catw <- function(..., callstack=sys.calls()){
  cs <- callstack
  cs <- clean_cs(cs)
  
  
  #browser()
  message(paste(cs, ...))
}

clean_cs <- function(x){
  val <- sapply(x, function(xt){
    z <- strsplit(paste(xt, collapse="\t"), "\t")[[1]]
    switch(z[1],
           "lapply" = z[3], 
           "sapply" = z[3],
           "do.call" = z[2], 
           "function" = "FUN",
           "source" = "###",
           "eval.with.vis" = "###",
           z[1]
    )
  })
  val[grepl("\\<function\\>", val)] <- "FUN"
  val <- val[!grepl("(###|FUN)", val)]
  val <- head(val, -1)
  paste(val, collapse="|")
}

# __Primary Coding Functions__ ================================================= 

# structure of functions with the prefix primary_coding_snid 
# The functions create one or more new columns (using mutate)
# the functions return the trial data including new columns
# in the last step of this script, in the function primary_coding_snid(), all selected primary_coding_snid steps are performed consecutively. 
# this is the function which will be called in the run script to create trial_data from trial_data_raw

## Age =========================================================================

#' Primary coding age
#'
#' adds the following columns to patinf: 
#' ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @import lubridate
#' @export

primary_coding_snid_age <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?patinf$", table_names)]] <- trial_data[[grep("^_?patinf$", table_names)]] %>% 
    # get date of signed informed consent
    dplyr::left_join(trial_data[[grep("^_?ic$", table_names)]] %>% select(mnppid,ic_infcons_date.date), by = "mnppid") %>%
    # birth date was only reported as year (YYYY), but needed to be YYYY-MM-DD --> we added -06-30 to set the date of birth to June 30th as middle of the respective year
    dplyr::mutate(ecu_patinf_birthyear_new = ymd(paste0(.data$patinf_birthyear, "-06-30")),
                  ecu_age = calculate_full_years(from = .data$ecu_patinf_birthyear_new, to = .data$ic_infcons_date.date),
                  ecu_age_cat_dec = ecu_age_cat_dec(.data$ecu_age),
                  ecu_age_cat_3 = ecu_age_cat_3(.data$ecu_age))
  
  labelled::var_label(trial_data[[grep("^_?patinf$", table_names)]]) <- list(
    ecu_age_cat_3 = "",
    ecu_age_cat_dec = ""
  )
  
  return(trial_data)
}

## BMI =========================================================================

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to patinf: 
#' ecu_bmi, ecu_bmi_cat, ecu_adipositas
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @importFrom forcats fct_collapse
#' @import dplyr
#' @export

primary_coding_snid_bmi <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?patinf$", table_names)]] <- trial_data[[grep("^_?patinf$", table_names)]] %>%
    dplyr::mutate(ecu_bmi = calculate_bmi(.data$patinf_weight, .data$patinf_height),
                  ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi),
                  ecu_bmi_adipositas = dplyr::case_when(!is.na(.data$ecu_bmi_cat) ~  forcats::fct_collapse(.data$ecu_bmi_cat,
                                                                                                           Ja = c("Adipositas Grad I", "Adipositas Grad II", "Adipositas Grad III"),
                                                                                                           Nein = c("Untergewicht", "Normalgewicht", "\u00dcbergewicht"))))
  
  labelled::var_label(trial_data[[grep("^_?patinf$", table_names)]]) <- list(
    ecu_bmi = "",
    ecu_bmi_cat = "",
    ecu_bmi_adipositas = ""
  )
  
  return (trial_data)
}

# SNID Wrapper primary coding ==========================================

#' Primary coding SNID Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#'  
#' @param trial_data The secu trial data object
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @importFrom forcats fct_reorder
#' @export

primary_coding_snid <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  table_names <- names(trial_data)
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?demo$", table_names)]]), "mnpvislabel", "visit_name")
  
  ## Age =======================================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_age(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_age() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## BMI =========================================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_bmi(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  catw("Primary Coding done")
  
  return(trial_data)
}
