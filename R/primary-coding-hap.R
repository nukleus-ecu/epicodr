#################
# Primary coding for NAPKON HAP secuTrial data via secuTrialR
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

# structure of functions with the prefix primary_coding_hap 
# The functions create one or more new columns (using mutate)
# the functions return the trial data including new variables
# in the last step of this script, in the function primary_coding_hap(), all selected primary_coding_hap steps are performed consecutively. 
# this is the function which will be called in the run script to create trial_data from trial_data_raw

# Demographics =================================================================

## Age =========================================================================

#' Primary coding age
#'
#' adds the following columns to _demo: ecu_age - age in years, ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_age <- function(trial_data) {
  
  trial_data[["demo"]] <- trial_data[["demo"]] %>%
    # HAP already has an age in years vector, simply copy it
    mutate(ecu_age = .data$demo_0010,
           ecu_age = ifelse(is.na(.data$ecu_age), .data$demo_0011/12, .data$ecu_age),
           ecu_age = ifelse(is.na(.data$ecu_age), .data$demo_0012/52, .data$ecu_age),
           ecu_age = ifelse(is.na(.data$ecu_age), .data$demo_0013/365, .data$ecu_age),
           # demo_0014 enthält 3 Alterskategorien kleiner-gleich 3 Jahre, aber hier nicht relevant. daher pauschal 3 Jahre
           ecu_age = ifelse(is.na(.data$ecu_age) & !is.na(.data$demo_0014), 3, .data$ecu_age)) %>%
    mutate(ecu_age_cat_dec = ecu_age_cat_dec(.data$demo_0010),
           ecu_age_cat_3 = ecu_age_cat_3(.data$demo_0010))
  
  return(trial_data)
}


## BMI =========================================================================

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to _demo: ecu_bmi, ecu_bmi_cat
#'
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_bmi <- function(trial_data) {
  
  trial_data[["demo"]] <- trial_data[["demo"]] %>%
    mutate(ecu_bmi = calculate_bmi(.data$demo_0031, .data$demo_0041),
           ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi))
  
  return(trial_data)
}


# Clinical Parameters (historic) ===============================================

# the following clinical parameters are categorized according to primary coding:
# blood pressure, heart frequency, oxygen saturation, respiration rate, temperature, gcs, horowitz-index, ph

# ============================================================================ #

#' Primary coding clinical parameters (historic)
#' 
#' adds the following variables to _vitalhis: 
#' ecu_vitals_his_bp, ecu_vitals_his_bpm, ecu_vitals_his_so2, ecu_vitals_his_resp_rate, ecu_vitals_his_temp,
#' ecu_vitals_his_gcs, ecu_vitals_his_horowitz_cat, ecu_vitals_his_ph
#'
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_clinical_params_his <- function(trial_data) {
  
  trial_data[["vitalhis"]] <- trial_data[["vitalhis"]] %>%
    mutate(ecu_vitals_his_bp = categorize_bloodpressure_ecu(.data$vp_0041, .data$vp_0042),
           ecu_vitals_his_bpm = categorize_heartfrequency_ecu(.data$vp_0031),
           ecu_vitals_his_so2 = categorize_oxigensaturation_ecu(.data$vp_0021), 
           ecu_vitals_his_resp_rate = categorize_resp_rate_ecu(.data$vp_0051),
           ecu_vitals_his_temp = categorize_temp_ecu(.data$vp_0061), 
           ecu_vitals_his_gcs = categorize_gcs_ecu(.data$vp_0081_ni), 
           ecu_vitals_his_horowitz_cat = categorize_horowitz_index_ecu(.data$vp_0192),
           ecu_vitals_his_ph = categorize_ph_ecu(.data$vp_0171))
  
  return(trial_data)
}


# Clinical Parameters =============================================================

# the following clinical parameters are categorized according to primary coding:
# blood pressure, heart frequency, oxygen saturation, breath rate, temperature, gcs, ph

# ============================================================================ #

#' Primary coding clinical parameters
#' 
#' adds the following variables to _vitalparam: 
#' ecu_vitals_bp, ecu_vitals_bpm, ecu_vitals_so2, ecu_vitals_resp_rate, ecu_vitals_temp,
#' ecu_vitals_gcs, ecu_vitals_ph
#'
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_clinical_params <- function(trial_data) {
  
  trial_data[["vitalparam"]] <- trial_data[["vitalparam"]] %>%
    mutate(ecu_vitals_bp = categorize_bloodpressure_ecu(.data$vp_0041, .data$vp_0042),
           ecu_vitals_bpm = categorize_heartfrequency_ecu(.data$vp_0031),
           ecu_vitals_so2 = categorize_oxigensaturation_ecu(.data$vp_0021), 
           ecu_vitals_resp_rate = categorize_resp_rate_ecu(.data$vp_0051),
           ecu_vitals_temp = categorize_temp_ecu(.data$vp_0061), 
           ecu_vitals_gcs = categorize_gcs_ecu(.data$vp_0081_ni), 
           ecu_vitals_ph = categorize_ph_ecu(.data$vp_0171))
  
  return(trial_data)
}


# Scores =======================================================================

# the following scores are categorized according to primary coding:
# Barthel Index, MoCa, EQ5D-5L, NEWS, APACHE-2, ICDSC, DDS, WHO-Scale, PCS-Score

# ============================================================================ #


#' Primary coding Barthel index prior to infection (baseline)
#' 
#' adds the following variable to _risiko1:
#' ecu_barthel_cat_pre
#' 
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_barthel_pre <- function(trial_data) {
  
  trial_data[["risiko1"]] <- trial_data[["risiko1"]] %>%
    mutate(ecu_barthel_cat_pre = categorize_barthel_ecu(.data$risk_0022))
  
  return (trial_data)
}


#' Primary coding Barthel index at discharge
#' 
#' adds the following variable to _end:
#' ecu_barthel_cat_disc
#' 
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_barthel_disc <- function(trial_data) {
  
  trial_data[["end"]] <- trial_data[["end"]] %>%
    mutate(ecu_barthel_cat_disc = categorize_barthel_ecu(.data$out_0062))
  
  return (trial_data)
}


#' Primary coding EQ5D-5L-Index
#' 
#' adds the following variable to _eq5d:
#' ecu_eq5d_index
#'
#' @param trial_data SecuTrial Data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_hap_eq5d5l <- function(trial_data) {
  trial_data[["eq5d"]] <- trial_data[["eq5d"]] %>%
    mutate (ecu_eq5d5l_index = calculate_eq5d5l_index (.data$eq5d_0020, .data$eq5d_0030, .data$eq5d_0040, .data$eq5d_0050, .data$eq5d_0060))
  
  return(trial_data)
}


#' Primary coding for National Early Warning Score (NEWS)
#' 
#' adds the following variable to _klinscores:
#' ecu_news_cat
#' 
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_news_first <- function(trial_data) {
  
  trial_data[["klinscores"]] <- trial_data[["klinscores"]] %>%
    mutate(ecu_news_cat = categorize_news_score_ecu(.data$score_0041))
  
  return(trial_data)
}


#' Primary coding for Acute Physiology And Chronic Health Evaluation (APACHE) Score
#' 
#' adds the following variable to _klinscores1:
#' ecu_apache2_cat
#' 
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_apache2 <- function(trial_data) {
  
  trial_data[["klinscores1"]] <- trial_data[["klinscores1"]] %>%
    mutate(ecu_apache2_cat = categorize_apache2_score_ecu(.data$icusc_0041))
  
  return(trial_data)
}


#' Primary coding for Intensive Care Delirium Screening Checklist (ICDSC)
#' 
#' adds the following variable to _haemodyn:
#' ecu_icdsc_cat
#' 
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_icdsc <- function(trial_data) {
  
  trial_data[["haemodyn"]] <- trial_data[["haemodyn"]] %>%
    mutate(ecu_icdsc_cat = categorize_icdsc_score_ecu(.data$ksc_0041))
  
  return(trial_data)
}


#' Primary coding for Delirium Detection Score (DDS)
#' 
#' adds the following variable to _haemodyn:
#' ecu_dds_cat
#' 
#' @param trial_data SecuTrial Data
#' @importFrom rlang .data
#' @export

primary_coding_hap_dds <- function(trial_data) {
  
  trial_data[["haemodyn"]] <- trial_data[["haemodyn"]] %>%
    mutate(ecu_dds_cat = categorize_dds_score_ecu(.data$ksc_0051))
  
  return(trial_data)
}



# HAP Wrapper primary coding ==================================================

#' Primary coding HAP Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#' 
#' 
#' @param trial_data The secu trial data object
#' @return The secu trial data object with primary coded variables
#' @export

primary_coding_hap <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. set_id_names() did not work.")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  # Demographics
  
  tryCatch(expr = {trial_data <- primary_coding_hap_age(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_age() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_bmi(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  # Clinical parameters
  tryCatch(expr = {trial_data <- primary_coding_hap_clinical_params_his(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_clinical_params_his() did not work. This is likely due to missing variables.")
             print(e)})
  
  tryCatch(expr = {trial_data <- primary_coding_hap_clinical_params(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_clinical_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  # Scores
  tryCatch(expr= {trial_data <- primary_coding_hap_barthel_pre(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_barthel_pre() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_barthel_disc(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_barthel_disc() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_eq5d5l(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_eq5d5l() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_news_first(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_news_first() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_apache2(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_apache2() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_icdsc(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_icdsc() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_dds(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_dds() did not work. This is likely due to missing variables.")
             print(e)})
  
  catw("Primary Coding done")
  
  return(trial_data)
}


# HAP helper functions for primary coding ==================================================