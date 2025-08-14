#################
# Primary coding for RAPID-REVIVE secuTrial data via secuTrialR =================
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

# structure of functions with the prefix primary_coding_rapid_revive 
# The functions create one or more new columns (using mutate)
# the functions return the trial data including new columns
# in the last step of this script, in the function primary_coding_pop(), all selected primary_coding_rapid_revive steps are performed consecutively. 
# this is the function which will be called in the run script to create trial_data from trial_data_raw

## Age =========================================================================

#' Primary coding age
#'
#' adds the following columns to demo: 
#' ecu_age - age in years, ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export
 
primary_coding_rapid_revive_age <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
    mutate(ecu_age_cat_dec = ecu_age_cat_dec(.data$demo_birth),
           ecu_age_cat_3 = ecu_age_cat_3(.data$demo_birth))
  
  labelled::var_label(trial_data[[grep("^_?demo$", table_names)]]) <- list(
    ecu_age_cat_3 = "",
    ecu_age_cat_dec = ""
  )
  
  return(trial_data)
}


## BMI =========================================================================

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to anthropo: 
#' ecu_bmi, ecu_bmi_cat, ecu_adipositas
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @importFrom forcats fct_collapse
#' @export

primary_coding_rapid_revive_bmi <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
    mutate(ecu_bmi = calculate_bmi(.data$demo_gewicht, .data$demo_height),
           ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi),
           ecu_bmi_adipositas = case_when(!is.na(.data$ecu_bmi_cat) ~  fct_collapse(.data$ecu_bmi_cat,
                                                                                    Ja = c("Adipositas Grad I", "Adipositas Grad II", "Adipositas Grad III"),
                                                                                    Nein = c("Untergewicht", "Normalgewicht", "\u00dcbergewicht"))))
  
  labelled::var_label(trial_data[["demo"]]) <- list(
    ecu_bmi = "",
    ecu_bmi_cat = "",
    ecu_bmi_adipositas = ""
  )
  
  return (trial_data)
}


## Blood pressure =========================================================================

#' Primary coding blood pressure
#'
#' adds the following columns to vital: 
#' ecu_bp
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_bp <- function(trial_data) {
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    mutate(ecu_bp = categorize_bloodpressure_ecu(.data$vital_sys, .data$vital_dia))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_bp = ""
  )
  
  return (trial_data)
}


# RAPID REVIVE Wrapper primary coding ==================================================

#' Primary coding RAPID REVIVE Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#'  
#' @param trial_data The secu trial data object
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @importFrom forcats fct_reorder
#' @export

primary_coding_rapid_revive <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?demo$", table_names)]]), "mnpvislabel", "visit_name")
  table_names <- names(trial_data)
  
  ## Age =======================================================================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_age(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_age() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## BMI =======================================================================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_bmi(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Blood pressure ============================================================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_bp(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_bp() did not work. This is likely due to missing variables.")
             print(e)})
  
  catw("Primary Coding done")
  
  return(trial_data)
}

