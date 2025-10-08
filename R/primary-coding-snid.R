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


## Heart frequency ==============================================================

#' Primary coding heart frequency
#'
#' adds the following columns to vital: 
#' ecu_hf
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_hf <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_hf = categorize_heartfrequency_ecu(.data$vital_freq))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_hf = ""
  )
  
  return (trial_data)
}


## Temperature ==============================================================

#' Primary coding temperature
#'
#' adds the following columns to vital: 
#' ecu_temp
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_temp <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_temp = categorize_temp_ecu(.data$vital_temp))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_temp = ""
  )
  
  return (trial_data)
}
## Respiration rate measurement (max of day) ==============================================================

#' Primary coding respiration rate
#'
#' adds the following columns to vital: 
#' ecu_resp_rate
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_resp_rate <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_resp_rate = categorize_resp_rate_ecu(.data$vital_resp))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_resp_rate = ""
  )
  
  return (trial_data)
}

## Oxygen Saturation (lowest) ==============================================================

#' Primary coding Oxygen Saturation
#'
#' adds the following columns to vital: 
#' ecu_oxy_sat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_oxy_sat <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_oxy_sat = categorize_oxigensaturation_ecu(.data$vital_spo2))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_oxy_sat = ""
  )
  
  return (trial_data)
}

## Glasgow Coma Scale ==============================================================

#' Primary coding Glasgow Coma Scale
#'
#' adds the following columns to vital: 
#' ecu_gcs
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_gcs <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_gcs = categorize_gcs_ecu(.data$vital_gcs))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_gcs = ""
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
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?patinf$", table_names)]]), "mnpvislabel", "visit_name")
  
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
  
  ## Heart frequency ============================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_hf(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_hf() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Temperature ============================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_temp(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_temp() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Respiration rate ============================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_resp_rate(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_resp_rate() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Oxygen saturation ============================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_oxy_sat(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_oxy_sat() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Glasgow Coma Scale ============================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_gcs(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_gcs() did not work. This is likely due to missing variables.")
             print(e)})
  
  
  catw("Primary Coding done")
  
  return(trial_data)
}
