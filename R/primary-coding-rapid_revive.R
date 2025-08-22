#################
# Primary coding for RAPID-REVIVE secuTrial data via secuTrialR ================
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
#' ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @import lubridate
#' @export
 
primary_coding_rapid_revive_age <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
    # birth date was only reported as year (YYYY), but needed to be YYYY-MM-DD --> we added -07-01 to set the birthdate to July 1st as middle of the respective year
    dplyr::mutate(ecu_demo_birth_new = ymd(paste0(.data$demo_birth, "-07-01")), 
                  ecu_age = calculate_full_years(from = .data$ecu_demo_birth_new, to = .data$demo_date.date),
                  ecu_age_cat_dec = ecu_age_cat_dec(.data$ecu_age),
                  ecu_age_cat_3 = ecu_age_cat_3(.data$ecu_age))
  
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
#' @import dplyr
#' @export

primary_coding_rapid_revive_bmi <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
    dplyr::mutate(ecu_bmi = calculate_bmi(.data$basis_gewicht, .data$demo_height),
                  ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi),
                  ecu_bmi_adipositas = dplyr::case_when(!is.na(.data$ecu_bmi_cat) ~  forcats::fct_collapse(.data$ecu_bmi_cat,
                                                                                           Ja = c("Adipositas Grad I", "Adipositas Grad II", "Adipositas Grad III"),
                                                                                           Nein = c("Untergewicht", "Normalgewicht", "\u00dcbergewicht"))))
  
  labelled::var_label(trial_data[["demo"]]) <- list(
    ecu_bmi = "",
    ecu_bmi_cat = "",
    ecu_bmi_adipositas = ""
  )
  
  return (trial_data)
}


## Blood pressure ==============================================================

#' Primary coding blood pressure
#'
#' adds the following columns to vital: 
#' ecu_bp
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_rapid_revive_bp <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_bp = categorize_bloodpressure_ecu(.data$vital_sys, .data$vital_dia))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_bp = ""
  )
  
  return (trial_data)
}

## Fatigue Severity Scale (FSS) ================================================

#' Primary coding Fatigue Severity Scale (FSS)
#' 
#' adds the following colums to fss
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_fss <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?fss$", table_names)]] <- trial_data[[grep("^_?fss$", table_names)]] %>%
    dplyr::mutate(ecu_fss_sum = calculate_fss_sum(.data$fss_001, .data$fss_002, .data$fss_003, .data$fss_004, .data$fss_005, .data$fss_006, .data$fss_007, .data$fss_008, .data$fss_009),
                  ecu_fss_cat = categorize_fss_sum_ecu(.data$ecu_fss_sum),
                  ecu_fss_mean = calculate_fss_mean(.data$fss_001, .data$fss_002, .data$fss_003, .data$fss_004, .data$fss_005, .data$fss_006, .data$fss_007, .data$fss_008, .data$fss_009),
                  ecu_fss_cat_2 = categorize_fss_mean_ecu(.data$ecu_fss_mean),
                  ecu_fss_sum_phys = calculate_fss_sum_phys(.data$fss_002, .data$fss_004, .data$fss_006),
                  ecu_fss_sum_mental = calculate_fss_sum_mental(.data$fss_001, .data$fss_003, .data$fss_005, .data$fss_007, .data$fss_008, .data$fss_009),
                  ecu_fss_mental_phys_ratio = round(.data$ecu_fss_sum_mental / .data$ecu_fss_sum_phys, digits = 2))
  
  labelled::var_label(trial_data[[grep("^_?fss$", table_names)]]) <- list(
    ecu_fss_sum = "",
    ecu_fss_cat = "",
    ecu_fss_mean = "",
    ecu_fss_cat_2 = "",
    ecu_fss_sum_phys = "",
    ecu_fss_sum_mental = "",
    ecu_fss_mental_phys_ratio = ""
  )
  
  return(trial_data)
  
}



# RAPID REVIVE Wrapper primary coding ==========================================

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
  
  table_names <- names(trial_data)
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?demo$", table_names)]]), "mnpvislabel", "visit_name")

  
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
  
  ## Fatigue Severity Scale (FSS) ==============================================
    tryCatch(expr = {trial_data <- primary_coding_rapid_revive_fss(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_fss() did not work. This is likely due to missing variables.")
             print(e)})
  
  catw("Primary Coding done")
  
  return(trial_data)
}

