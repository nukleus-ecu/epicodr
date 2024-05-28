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
#' adds the following columns to demo: 
#' ecu_age - age in years, ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_age <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  if ("demo_0011" %in% names(trial_data[[grep("^_?demo$", table_names)]]) & "demo_0012" %in% names(trial_data[[grep("^_?demo$", table_names)]]) & 
      "demo_0013" %in% names(trial_data[[grep("^_?demo$", table_names)]]) & "demo_0014" %in% names(trial_data[[grep("^_?demo$", table_names)]])) {trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
      # HAP already has an age in years vector, simply copy it
      mutate(ecu_age = .data$demo_0010,
             ecu_age = ifelse(is.na(.data$ecu_age), .data$demo_0011/12, .data$ecu_age),
             ecu_age = ifelse(is.na(.data$ecu_age), .data$demo_0012/52, .data$ecu_age),
             ecu_age = ifelse(is.na(.data$ecu_age), .data$demo_0013/365, .data$ecu_age),
             # demo_0014 enthält 3 Alterskategorien kleiner-gleich 3 Jahre, aber hier nicht relevant. daher pauschal 3 Jahre
             ecu_age = ifelse(is.na(.data$ecu_age) & !is.na(.data$demo_0014), 3, .data$ecu_age)) %>%
      mutate(ecu_age_cat_dec = ecu_age_cat_dec(.data$demo_0010),
             ecu_age_cat_3 = ecu_age_cat_3(.data$demo_0010))} else {
               trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
                 # HAP already has an age in years vector, simply copy it
                 mutate(ecu_age = .data$demo_0010,
                        ecu_age_cat_dec = ecu_age_cat_dec(.data$demo_0010),
                        ecu_age_cat_3 = ecu_age_cat_3(.data$demo_0010))
             }
  
  labelled::var_label(trial_data[[grep("^_?demo$", table_names)]]) <- list(
    ecu_age = "",
    ecu_age_cat_dec = "",
    ecu_age_cat_3 = ""
  )
  
  return(trial_data)
}


## BMI =========================================================================

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to demo: 
#' ecu_bmi, ecu_bmi_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_bmi <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?demo$", table_names)]] <- trial_data[[grep("^_?demo$", table_names)]] %>%
    mutate(ecu_bmi = calculate_bmi(.data$demo_0031, .data$demo_0041),
           ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi))
  
  labelled::var_label(trial_data[[grep("^_?demo$", table_names)]]) <- list(
    ecu_bmi = "",
    ecu_bmi_cat = ""
  )
  
  return(trial_data)
}


# Clinical Parameters (historic) ===============================================

# the following clinical parameters are categorized according to primary coding:
# blood pressure, heart frequency, oxygen saturation, respiration rate, temperature, gcs, horowitz-index, ph

# ============================================================================ #

#' Primary coding clinical parameters (historic)
#' 
#' adds the following columns to vitalhis: 
#' ecu_vitals_his_bp, ecu_vitals_his_bpm, ecu_vitals_his_so2, ecu_vitals_his_resp_rate, ecu_vitals_his_temp,
#' ecu_vitals_his_gcs, ecu_vitals_his_horowitz_cat, ecu_vitals_his_ph
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_clinical_params_his <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vitalhis$", table_names)]] <- trial_data[[grep("^_?vitalhis$", table_names)]] %>%
    mutate(ecu_vitals_his_bp = categorize_bloodpressure_ecu(.data$vp_0041, .data$vp_0042),
           ecu_vitals_his_bpm = categorize_heartfrequency_ecu(.data$vp_0031),
           ecu_vitals_his_so2 = categorize_oxigensaturation_ecu(.data$vp_0021), 
           ecu_vitals_his_resp_rate = categorize_resp_rate_ecu(.data$vp_0051),
           ecu_vitals_his_temp = categorize_temp_ecu(.data$vp_0061), 
           ecu_vitals_his_gcs = categorize_gcs_ecu(.data$vp_0081_ni), 
           ecu_vitals_his_horowitz_cat = categorize_horowitz_index_ecu(.data$vp_0192),
           ecu_vitals_his_ph = categorize_ph_ecu(.data$vp_0171))
  
  labelled::var_label(trial_data[[grep("^_?vitalhis$", table_names)]]) <- list(
    ecu_vitals_his_bp = "",
    ecu_vitals_his_bpm = "",
    ecu_vitals_his_so2 = "", 
    ecu_vitals_his_resp_rate = "",
    ecu_vitals_his_temp = "", 
    ecu_vitals_his_gcs = "", 
    ecu_vitals_his_horowitz_cat = "",
    ecu_vitals_his_ph = ""
  )
  
  return(trial_data)
}


# Clinical Parameters =============================================================

# the following clinical parameters are categorized according to primary coding:
# blood pressure, heart frequency, oxygen saturation, breath rate, temperature, gcs, ph

# ============================================================================ #

#' Primary coding clinical parameters
#' 
#' adds the following columns to vitalparam: 
#' ecu_vitals_bp, ecu_vitals_bpm, ecu_vitals_so2, ecu_vitals_resp_rate, ecu_vitals_temp,
#' ecu_vitals_gcs, ecu_vitals_ph
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_clinical_params <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vitalparam$", table_names)]] <- trial_data[[grep("^_?vitalparam$", table_names)]] %>%
    mutate(ecu_vitals_bp = categorize_bloodpressure_ecu(.data$vp_0041, .data$vp_0042),
           ecu_vitals_bpm = categorize_heartfrequency_ecu(.data$vp_0031),
           ecu_vitals_so2 = categorize_oxigensaturation_ecu(.data$vp_0021), 
           ecu_vitals_resp_rate = categorize_resp_rate_ecu(.data$vp_0051),
           ecu_vitals_temp = categorize_temp_ecu(.data$vp_0061), 
           ecu_vitals_gcs = categorize_gcs_ecu(.data$vp_0081_ni), 
           ecu_vitals_ph = categorize_ph_ecu(.data$vp_0171))
  
  labelled::var_label(trial_data[[grep("^_?vitalparam$", table_names)]]) <- list(
    ecu_vitals_bp = "",
    ecu_vitals_bpm = "",
    ecu_vitals_so2 = "", 
    ecu_vitals_resp_rate = "",
    ecu_vitals_temp = "", 
    ecu_vitals_gcs = "", 
    ecu_vitals_ph = ""
  )
  
  return(trial_data)
}


# Scores =======================================================================

# the following scores are categorized according to primary coding:
# Barthel Index, EQ5D-5L, MoCA, NEWS, APACHE-2, ICDSC, DDS, WHO-Scale

# ============================================================================ #


#' Primary coding Barthel index prior to infection (baseline)
#' 
#' adds the following column to risiko1:
#' ecu_barthel_cat_pre
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_barthel_pre <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?risiko1$", table_names)]] <- trial_data[[grep("^_?risiko1$", table_names)]] %>%
    mutate(ecu_barthel_cat_pre = categorize_barthel_ecu(.data$risk_0022))
  
  labelled::var_label(trial_data[[grep("^_?risiko1$", table_names)]]) <- list(
    ecu_barthel_cat_pre = ""
  )
  
  return (trial_data)
}


#' Primary coding Barthel index at discharge
#' 
#' adds the following column to end:
#' ecu_barthel_cat_disc
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_barthel_disc <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?end$", table_names)]] <- trial_data[[grep("^_?end$", table_names)]] %>%
    mutate(ecu_barthel_cat_disc = categorize_barthel_ecu(.data$out_0062))
  
  labelled::var_label(trial_data[[grep("^_?end$", table_names)]]) <- list(
    ecu_barthel_cat_disc = ""
  )
  
  return (trial_data)
}


#' Primary coding EQ5D-5L-Index
#' 
#' adds the following column to eq5d:
#' ecu_eq5d_index
#'
#' @param trial_data A secuTrial data object
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_hap_eq5d5l <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?eq5d$", table_names)]] <- trial_data[[grep("^_?eq5d$", table_names)]] %>%
    mutate (ecu_eq5d5l_index = calculate_eq5d5l_index (.data$eq5d_0020, .data$eq5d_0030, .data$eq5d_0040, .data$eq5d_0050, .data$eq5d_0060))
  
  labelled::var_label(trial_data[[grep("^_?eq5d$", table_names)]]) <- list(
    ecu_eq5d5l_index = ""
  )
  
  return(trial_data)
}


#' Primary cpding for Montreal Cognitive Assessment (MoCA)
#' 
#' adds the following varible to moca
#' ecu_moca_total_score, ecu_moca_cat
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_moca <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?moca$", table_names)]] <- trial_data[[grep("^_?moca$", table_names)]] %>%
    mutate(ecu_moca_total_score = ifelse(.data$moca_0021 == 1, .data$moca_0022, 0) +
             ifelse(.data$moca_0023 == 1, .data$moca_0024, 0) +
             ifelse(.data$moca_0025 == 1, .data$moca_0026, 0) +
             .data$moca_0032 +
             ifelse(.data$moca_0051 == 1, .data$moca_0052, 0) +
             ifelse(.data$moca_0053 == 1, .data$moca_0054, 0) +
             ifelse(.data$moca_0055 == 1, .data$moca_0056, 0) +
             ifelse(.data$moca_0057 == 1, .data$moca_0058, 0) +
             ifelse(.data$moca_0061 == 1, .data$moca_0062, 0) +
             ifelse(.data$moca_0063 == 1, .data$moca_0064, 0) +
             ifelse(is.na(.data$moca_0072), .data$moca_0071, 0) + 
             ifelse(.data$moca_0081 == 1, .data$moca_0082, 0) +
             ifelse(.data$moca_0091 == 1, .data$moca_0092, 0) +
             ifelse(.data$moca_0097 == 1, .data$moca_0098, 0) +
             ifelse(.data$moca_0093 == 1, .data$moca_0094, 0) +
             ifelse(.data$moca_0095 == 1, .data$moca_0096, 0) +
             ifelse(.data$moca_0099 == 1, .data$moca_0100, 0) +
             ifelse(.data$moca_0101 == 1, .data$moca_0102, 0) +
             ifelse(.data$moca_0110 >= 0, .data$moca_0110, 0),
           ecu_moca_cat = categorize_moca_ecu(.data$ecu_moca_total_score)) 
  
  labelled::var_label(trial_data[[grep("^_?moca$", table_names)]]) <- list(
    ecu_moca_total_score = "",
    ecu_moca_cat = ""
  )
  
  return(trial_data)
}


#' Primary coding for National Early Warning Score (NEWS)
#' 
#' adds the following column to klinscores:
#' ecu_news_cat
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_news_first <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?klinscores$", table_names)]] <- trial_data[[grep("^_?klinscores$", table_names)]] %>%
    mutate(ecu_news_cat = categorize_news_score_ecu(.data$score_0041))
  
  labelled::var_label(trial_data[[grep("^_?klinscores$", table_names)]]) <- list(
    ecu_news_cat = ""
  )
  
  return(trial_data)
}


#' Primary coding for Acute Physiology And Chronic Health Evaluation (APACHE) Score
#' 
#' adds the following column to klinscores1:
#' ecu_apache2_cat
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_apache2 <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?klinscores1$", table_names)]] <- trial_data[[grep("^_?klinscores1$", table_names)]]%>%
    mutate(ecu_apache2_cat = categorize_apache2_score_ecu(.data$icusc_0041))
  
  labelled::var_label(trial_data[[grep("^_?klinscores1$", table_names)]]) <- list(
    ecu_apache2_cat = ""
  )
  
  return(trial_data)
}


#' Primary coding for Intensive Care Delirium Screening Checklist (ICDSC)
#' 
#' adds the following column to haemodyn:
#' ecu_icdsc_cat
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_icdsc <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?haemodyn$", table_names)]] <- trial_data[[grep("^_?haemodyn$", table_names)]] %>%
    mutate(ecu_icdsc_cat = categorize_icdsc_score_ecu(.data$ksc_0041))
  
  labelled::var_label(trial_data[[grep("^_?haemodyn$", table_names)]]) <- list(
    ecu_icdsc_cat = ""
  )
  
  return(trial_data)
}


#' Primary coding for Delirium Detection Score (DDS)
#' 
#' adds the following column to haemodyn:
#' ecu_dds_cat
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_dds <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?haemodyn$", table_names)]] <- trial_data[[grep("^_?haemodyn$", table_names)]] %>%
    mutate(ecu_dds_cat = categorize_dds_score_ecu(.data$ksc_0051))
  
  labelled::var_label(trial_data[[grep("^_?haemodyn$", table_names)]]) <- list(
    ecu_dds_cat = ""
  )
  
  return(trial_data)
}


#' Primary coding WHO-Scale
#' 
#' adds the following columns to eosfci
#' ecu_who_scale.factor, ecu_who_scale
#' 
#' adds the following columns to osfci
#' ecu_who_scale_max.factor, ecu_who_scale_max
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_who_scale <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. set_id_names() did not work.")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  
  trial_data <- build_who_scale_hap(trial_data, pid)
  
  return(trial_data)
}


#' Primary coding Fatigue Severity Scale (FSS)
#' 
#' adds the following colums to fss
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_hap_fss <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?fss$", table_names)]] <- trial_data[[grep("^_?fss$", table_names)]] %>%
    mutate(ecu_fss_sum = calculate_fss_sum(.data$fss_0021, .data$fss_0022, .data$fss_0023, .data$fss_0024, .data$fss_0025, .data$fss_0026, .data$fss_0027, .data$fss_0028, .data$fss_0029),
           ecu_fss_cat = categorize_fss_sum_ecu(.data$ecu_fss_sum),
           ecu_fss_mean = calculate_fss_mean(.data$fss_0021, .data$fss_0022, .data$fss_0023, .data$fss_0024, .data$fss_0025, .data$fss_0026, .data$fss_0027, .data$fss_0028, .data$fss_0029),
           ecu_fss_cat_2 = categorize_fss_mean_ecu(.data$ecu_fss_mean),
           ecu_fss_sum_phys = calculate_fss_sum_phys(.data$fss_0022, .data$fss_0024, .data$fss_0026),
           ecu_fss_sum_mental = calculate_fss_sum_mental(.data$fss_0021, .data$fss_0023, .data$fss_0025, .data$fss_0027, .data$fss_0028, .data$fss_0029),
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


#' Primary coding Post-COVID Score (PCS)
#' 
#' adds the following dataframes to data:
#' ecu_long_symptom_data, ecu_pcs_score
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @param prom A vector indication whether proms should be included in pcs calculation or not; needs to be specified as prom = "No" or prom = "Yes"
#' @return A secuTrial data object with primary coded variables and dataframes
#' @export

primary_coding_hap_pcs_score <- function(trial_data, prom = "No") {
  
  table_names <- names(trial_data)
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. set_id_names() did not work.")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  visitid <- trial_data$export_options$id_names$visitid
  
  trial_data[["ecu_long_symptom_data"]] <- build_hap_long_symptom_df(trial_data, pid) 
  
  if (prom == "No") {trial_data[["ecu_pcs_score"]] <- build_hap_pcs_score_df_without_proms(trial_data, pid)}
  else if (prom == "Yes") {trial_data[["ecu_pcs_score"]] <- build_hap_pcs_score_df_with_proms(trial_data, pid)}
  
  return(trial_data)
  
}


# HAP Wrapper primary coding ==================================================

#' Primary coding HAP Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#' 
#' 
#' @param trial_data The secuTrial data object
#' @return The secuTrial data object with primary coded variables
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
  
  table_names <- names(trial_data)
  
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  
  ## Demographics ==============================================================
  ### Age ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_age(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_age() did not work. This is likely due to missing variables.")
             print(e)})
  ### BMI ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_bmi(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Clinical parameters =======================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_clinical_params_his(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_clinical_params_his() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_clinical_params(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_clinical_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Scores ====================================================================
  ### Barthel-Index ============================================================
  tryCatch(expr= {trial_data <- primary_coding_hap_barthel_pre(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_barthel_pre() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_hap_barthel_disc(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_barthel_disc() did not work. This is likely due to missing variables.")
             print(e)})
  ### EQ-5D-5L =================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_eq5d5l(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_eq5d5l() did not work. This is likely due to missing variables.")
             print(e)})
  ### MoCA =====================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_moca(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_moca() did not work. This is likely due to missing variables.")
             print(e)})
  ### NEWS-Score ===============================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_news_first(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_news_first() did not work. This is likely due to missing variables.")
             print(e)})
  ### APACHE-2 Score ===========================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_apache2(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_apache2() did not work. This is likely due to missing variables.")
             print(e)})
  ### ICDSC Score ==============================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_icdsc(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_icdsc() did not work. This is likely due to missing variables.")
             print(e)})
  ### DDS Score ================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_dds(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_dds() did not work. This is likely due to missing variables.")
             print(e)})
  ### WHO-Scale ================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_who_scale(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_who_scale() did not work. This is likely due to missing variables.")
             print(e)})
  ### Fatigue Severity Scale ================================================================
  tryCatch(expr = {trial_data <- primary_coding_hap_fss(trial_data)},
           error = function(e) {
             warning("primary_coding_hap_fss() did not work. This is likely due to missing variables.")
             print(e)})

  catw("Primary Coding done")
  
  return(trial_data)
}


# HAP helper functions for primary coding ==================================================

## WHO-Scale ===================================================================

#' Calculate and categorize WHO-Scale
#' 
#' adds the following columns to eosfci
#' ecu_who_scale.factor, ecu_who_scale
#' 
#' adds the following columns to osfci
#' ecu_who_scale_max.factor, ecu_who_scale_max
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @importFrom rlang .data
#' @export

build_who_scale_hap <- function(trial_data, pid) {
  
  table_names <- names(trial_data)
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  
  main_diag <-  trial_data[[grep("^_?visit_02$", table_names)]] %>%
    filter(!!sym(visit_label_var_name) == "Screening / V1") %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$ea_0010) %>%
    ungroup() %>%
    select(!!sym(pid), "ea_0010")

  trial_data[[grep("^_?e_?osfci$", table_names)]] <-  trial_data[[grep("^_?e_?osfci$", table_names)]] %>%
    left_join(main_diag, by = pid) %>%
    mutate(ecu_who_scale.factor = case_when(is.na(.data$osfci_0021) ~ NA, 
                                            .data$osfci_0021 == 0 ~ "Kontrollgruppe, ohne Sars-Infektion",
                                            .data$osfci_0021 == 1 | .data$osfci_0021 == 2 ~ "Ambulant, milde Phase",
                                            .data$osfci_0021 == 3 | .data$osfci_0021 == 4 ~ "Hospitalisiert, moderate Phase",
                                            .data$osfci_0021 == 5 | .data$osfci_0021 == 6 | .data$osfci_0021 == 7 ~ "Hospitalisiert, schwere Phase",
                                            .data$osfci_0021 == 8 ~ "Verstorben"),
           ecu_who_scale = as.integer(case_when(.data$ecu_who_scale.factor == "Kontrollgruppe, ohne Sars-Infektion" ~ 0,
                                                .data$ecu_who_scale.factor == "Ambulant, milde Phase" ~ 1,
                                                .data$ecu_who_scale.factor == "Hospitalisiert, moderate Phase" ~ 2,
                                                .data$ecu_who_scale.factor == "Hospitalisiert, schwere Phase" ~ 3,
                                                .data$ecu_who_scale.factor == "Verstorben" ~ 4)),
           ecu_who_scale_with_diag.factor = case_when(is.na(.data$osfci_0021) | is.na(.data$ea_0010) ~ NA, 
                                                      ecu_who_scale == 2 & .data$ea_0010 == 1 ~ "Hospitalisiert wegen Covid, moderate Phase", 
                                                      ecu_who_scale == 2 & .data$ea_0010 == 0 ~ "Hospitalisiert mit Covid, moderate Phase",
                                                      ecu_who_scale == 3 & .data$ea_0010 == 1 ~ "Hospitalisiert wegen Covid, schwere Phase", 
                                                      ecu_who_scale == 3 & .data$ea_0010 == 0 ~ "Hospitalisiert mit Covid, schwere Phase",
                                                      TRUE ~ ecu_who_scale.factor),
           ecu_who_scale_with_diag = as.integer(case_when(.data$ecu_who_scale_with_diag.factor == "Kontrollgruppe, ohne Sars-Infektion" ~ 0,
                                                          .data$ecu_who_scale_with_diag.factor == "Ambulant, milde Phase" ~ 1,
                                                          .data$ecu_who_scale_with_diag.factor == "Hospitalisiert mit Covid, moderate Phase" ~ 2,
                                                          .data$ecu_who_scale_with_diag.factor == "Hospitalisiert wegen Covid, moderate Phase" ~ 3,
                                                          .data$ecu_who_scale_with_diag.factor == "Hospitalisiert mit Covid, schwere Phase" ~ 4,
                                                          .data$ecu_who_scale_with_diag.factor == "Hospitalisiert wegen Covid, schwere Phase" ~ 5,
                                                          .data$ecu_who_scale_with_diag.factor == "Verstorben" ~ 6)))
  
  labelled::var_label(trial_data[[grep("^_?e_?osfci$", table_names)]]) <- list(
    ecu_who_scale.factor = "",
    ecu_who_scale = "",
    ecu_who_scale_with_diag.factor = "",
    ecu_who_scale_with_diag = ""
  )
  
  #select(-(.data$ea_0010))
  
  who_scale_max <-  trial_data[[grep("^_?e_?osfci$", table_names)]] %>%
    select(!!sym(pid), "ecu_who_scale", "ecu_who_scale.factor") %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$ecu_who_scale, with_ties = FALSE) %>%
    distinct() %>%
    ungroup() %>%
    rename(ecu_who_scale_max.factor = "ecu_who_scale.factor",
           ecu_who_scale_max = "ecu_who_scale")

  who_scale_with_diag_max <-  trial_data[[grep("^_?e_?osfci$", table_names)]] %>%
    select(!!sym(pid), "ecu_who_scale_with_diag", "ecu_who_scale_with_diag.factor") %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$ecu_who_scale_with_diag, with_ties = FALSE) %>%
    distinct() %>%
    ungroup() %>%
    rename(ecu_who_scale_max_with_diag.factor = "ecu_who_scale_with_diag.factor",
           ecu_who_scale_max_with_diag = "ecu_who_scale_with_diag")

  trial_data[[grep("^_?osfci$", table_names)]] <-  trial_data[[grep("^_?osfci$", table_names)]] %>%
    left_join(who_scale_max, by = pid) %>%
    left_join(who_scale_with_diag_max, by = pid)
  
  labelled::var_label(trial_data[[grep("^_?osfci$", table_names)]]) <- list(
    ecu_who_scale_max = "",
    ecu_who_scale_max.factor = "",
    ecu_who_scale_max_with_diag = "",
    ecu_who_scale_max_with_diag.factor = ""
  )
  
  return(trial_data)
  
}


## Long Symptom Data Frame ============================================

#' Create long symptom data frame
#' 
#' returns a dataframe, which contains the 'long_symptom_data'
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in data
#' @importFrom rlang .data
#' 
#' @return A dataframe with one row per symptom per patient
#' @export

build_hap_long_symptom_df <- function(trial_data, pid) {
  
  table_names <- names(trial_data)
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  
  ecu_long_symptom_data <- trial_data[[grep("^_?symp$", table_names)]] %>%
    full_join(trial_data[[grep("^_?symp_cm$", table_names)]]) %>%
    select(all_of(pid):"sym_0010.factor", all_of(visit_label_var_name)) %>%
    left_join(trial_data[[grep("^_?e_?symptom$", table_names)]], by = c(pid, visit_label_var_name)) %>%
    select(all_of(pid):"sym_0021.factor", all_of(visit_label_var_name))
  
  return(ecu_long_symptom_data)
  
}


## Post-COVID Score (PCS) =============================================

#' Calculate symptom complex for Post-COVID Score (PCS) calculation
#' 
#' @param ecu_long_symptom_data A dataframe containing long_symptom_data
#' @param vector_of_pcss_fup_visit A vector with labels of desired visit
#' @param pid column name of patient ID in data
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' 
#' @return A dataframe containing symptom complexes per visit in vector_of_pcss_fup_visit
#' @export

detect_symptom_complex_hap <- function(ecu_long_symptom_data, vector_of_pcss_fup_visit, pid, trial_data) {
  
  table_names <- names(trial_data)
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  
  ecu_long_symptom_data <- ecu_long_symptom_data %>%
    mutate(complex_1_chemo = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                       .data$sym_0021.factor %in% c("Ver\u00e4nderter Geruchs- oder Geschmackssinn") ~ 1, 
                                       is.na(.data$sym_0021.factor) ~ NA_real_, 
                                       TRUE ~ 0),
           complex_2_fatigue = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                         .data$sym_0021.factor %in% c("Abgeschlagenheit / Ersch\u00f6pfung / Excessive tiredness") ~ 1, 
                                         is.na(.data$sym_0021.factor) ~ NA_real_, 
                                         TRUE ~ 0), 
           complex_3_exercise = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                          .data$sym_0021.factor %in% c("Kurzatmigkeit (Dyspnoe)") ~ 1, 
                                          is.na(.data$sym_0021.factor) ~ NA_real_, 
                                          TRUE ~ 0), 
           complex_4_pain = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                      .data$sym_0021.factor %in% c("Gelenkschmerzen (Arthralgie)", "Muskelschmerzen (Myalgie)") ~ 1, 
                                      is.na(.data$sym_0021.factor) ~ NA_real_, 
                                      TRUE ~ 0),
           complex_5_ent = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                     .data$sym_0021.factor %in% c("Halsschmerzen", "Laufende Nase (Rhinorrhoe)", "Niesen", "Verstopfte Nase") ~ 1, 
                                     is.na(.data$sym_0021.factor) ~ NA_real_, 
                                     TRUE ~ 0),
           complex_6_cough = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                       .data$sym_0021.factor %in% c("Husten", "Keuchen") ~ 1, 
                                       is.na(.data$sym_0021.factor) ~ NA_real_, 
                                       TRUE ~ 0),
           complex_7_chest = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                       .data$sym_0021.factor %in% c("Brustschmerz") ~ 1, 
                                       is.na(.data$sym_0021.factor) ~ NA_real_, 
                                       TRUE ~ 0),
           complex_8_gastro = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                        .data$sym_0021.factor %in% c("Bauchschmerzen", "Durchfall (Diarrh\u00f6e)", "Erbrechen", "\u00dcbelkeit") ~ 1, 
                                        is.na(.data$sym_0021.factor) ~ NA_real_, 
                                        TRUE ~ 0),
           complex_9_neuro = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                       .data$sym_0021.factor %in% c("Bewusstseinsst\u00f6rungen / Verwirrtheit / Orientierungsst\u00f6rung", "Konzentrationsst\u00f6rung",
                                                              "Schwindel", "Kopfschmerzen", "L\u00e4hmungen / Paresen") ~ 1, 
                                       is.na(.data$sym_0021.factor) ~ NA_real_, 
                                       TRUE ~ 0),
           complex_10_derma = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                        .data$sym_0021.factor %in% c("Haarausfall", "Hautver\u00e4nderung") ~ 1, 
                                        is.na(.data$sym_0021.factor) ~ NA_real_, 
                                        TRUE ~ 0),
           complex_11_flulike = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                          .data$sym_0021.factor %in% c("Appetitlosigkeit (Inappetenz)", "Fieber", "Lymphknotenschwellung (Lymphadenopathie)") ~ 1, 
                                          is.na(.data$sym_0021.factor) ~ NA_real_, 
                                          TRUE ~ 0),
           complex_12_sleep = case_when(.data$sym_0010.factor == "Ja, aber Patient war beschwerdefrei." & is.na(.data$sym_0021.factor) ~ 0, 
                                        .data$sym_0021.factor %in% c("Schlafst\u00f6rungen") ~ 1, 
                                        is.na(.data$sym_0021.factor) ~ NA_real_, 
                                        TRUE ~ 0))
  
  ecu_symptom_complex_per_visit <- ecu_long_symptom_data %>%
    group_by(!!sym(pid), !!sym(visit_label_var_name)) %>%
    summarise(complex_1_chemo_sum = case_when(any(.data$complex_1_chemo == 1) ~ 1,
                                              any(.data$complex_1_chemo == 0) ~ 0),
              complex_2_fatigue_sum = case_when(any(.data$complex_2_fatigue == 1) ~ 1, 
                                                any(.data$complex_2_fatigue == 0) ~ 0),
              complex_3_exercise_sum = case_when(any(.data$complex_3_exercise == 1) ~ 1, 
                                                 any(.data$complex_3_exercise == 0) ~ 0),
              complex_4_pain_sum = case_when(any(.data$complex_4_pain == 1) ~ 1, 
                                             any(.data$complex_4_pain == 0) ~ 0),
              complex_5_ent_sum = case_when(any(.data$complex_5_ent == 1) ~ 1, 
                                            any(.data$complex_5_ent == 0) ~ 0),
              complex_6_cough_sum = case_when(any(.data$complex_6_cough == 1) ~ 1, 
                                              any(.data$complex_6_cough == 0) ~ 0),
              complex_7_chest_sum = case_when(any(.data$complex_7_chest == 1) ~ 1, 
                                              any(.data$complex_7_chest == 0) ~ 0),
              complex_8_gastro_sum = case_when(any(.data$complex_8_gastro == 1) ~ 1, 
                                               any(.data$complex_8_gastro == 0) ~ 0),
              complex_9_neuro_sum = case_when(any(.data$complex_9_neuro == 1) ~ 1, 
                                              any(.data$complex_9_neuro == 0) ~ 0),
              complex_10_derma_sum = case_when(any(.data$complex_10_derma == 1) ~ 1, 
                                               any(.data$complex_10_derma == 0) ~ 0),
              complex_11_flulike_sum = case_when(any(.data$complex_11_flulike == 1) ~ 1, 
                                                 any(.data$complex_11_flulike == 0) ~ 0),
              complex_12_sleep_sum = case_when(any(.data$complex_12_sleep == 1) ~ 1, 
                                               any(.data$complex_12_sleep == 0) ~ 0)) %>%
    ungroup()
  
  ecu_symptom_complex_acute <- ecu_symptom_complex_per_visit %>%
    filter(str_detect(!!sym(visit_label_var_name), "Screening")) %>%
    group_by(!!sym(pid)) %>%
    summarise(complex_1_chemo_sum_acute = case_when(any(.data$complex_1_chemo_sum == 1) ~ 1,
                                                    any(.data$complex_1_chemo_sum == 0) ~ 0),
              complex_2_fatigue_sum_acute = case_when(any(.data$complex_2_fatigue_sum == 1) ~ 1, 
                                                      any(.data$complex_2_fatigue_sum == 0) ~ 0),
              complex_3_exercise_sum_acute = case_when(any(.data$complex_3_exercise_sum == 1) ~ 1, 
                                                       any(.data$complex_3_exercise_sum == 0) ~ 0),
              complex_4_pain_sum_acute = case_when(any(.data$complex_4_pain_sum == 1) ~ 1, 
                                                   any(.data$complex_4_pain_sum == 0) ~ 0),
              complex_5_ent_sum_acute = case_when(any(.data$complex_5_ent_sum == 1) ~ 1, 
                                                  any(.data$complex_5_ent_sum == 0) ~ 0),
              complex_6_cough_sum_acute = case_when(any(.data$complex_6_cough_sum == 1) ~ 1, 
                                                    any(.data$complex_6_cough_sum == 0) ~ 0),
              complex_7_chest_sum_acute = case_when(any(.data$complex_7_chest_sum == 1) ~ 1, 
                                                    any(.data$complex_7_chest_sum == 0) ~ 0),
              complex_8_gastro_sum_acute = case_when(any(.data$complex_8_gastro_sum == 1) ~ 1, 
                                                     any(.data$complex_8_gastro_sum == 0) ~ 0),
              complex_9_neuro_sum_acute = case_when(any(.data$complex_9_neuro_sum == 1) ~ 1, 
                                                    any(.data$complex_9_neuro_sum == 0) ~ 0),
              complex_10_derma_sum_acute = case_when(any(.data$complex_10_derma_sum == 1) ~ 1, 
                                                     any(.data$complex_10_derma_sum == 0) ~ 0),
              complex_11_flulike_sum_acute = case_when(any(.data$complex_11_flulike_sum == 1) ~ 1, 
                                                       any(.data$complex_11_flulike_sum == 0) ~ 0),
              complex_12_sleep_sum_acute = case_when(any(.data$complex_12_sleep_sum == 1) ~ 1, 
                                                     any(.data$complex_12_sleep_sum == 0) ~ 0)) %>%
    ungroup() 
  
  ecu_symptom_complex_fu <- ecu_symptom_complex_per_visit %>%
    filter(!!sym(visit_label_var_name) %in% vector_of_pcss_fup_visit) %>%
    rename(complex_1_chemo_sum_fu = .data$complex_1_chemo_sum,
           complex_2_fatigue_sum_fu = .data$complex_2_fatigue_sum,
           complex_3_exercise_sum_fu = .data$complex_3_exercise_sum,
           complex_4_pain_sum_fu = .data$complex_4_pain_sum,
           complex_5_ent_sum_fu = .data$complex_5_ent_sum,
           complex_6_cough_sum_fu = .data$complex_6_cough_sum,
           complex_7_chest_sum_fu = .data$complex_7_chest_sum,
           complex_8_gastro_sum_fu = .data$complex_8_gastro_sum,
           complex_9_neuro_sum_fu = .data$complex_9_neuro_sum,
           complex_10_derma_sum_fu = .data$complex_10_derma_sum,
           complex_11_flulike_sum_fu = .data$complex_11_flulike_sum,
           complex_12_sleep_sum_fu = .data$complex_12_sleep_sum)
  
  ecu_symptom_complex <- ecu_symptom_complex_acute %>%
    left_join(ecu_symptom_complex_fu, by = pid) %>%
    mutate(complex_1_chemo_final = case_when(.data$complex_1_chemo_sum_acute == 1 & .data$complex_1_chemo_sum_fu == 1 ~ 1,
                                             !is.na(.data$complex_1_chemo_sum_acute) & !is.na(.data$complex_1_chemo_sum_fu) ~ 0, 
                                             TRUE ~ NA_real_),
           complex_2_fatigue_final = case_when(.data$complex_2_fatigue_sum_acute == 1 & .data$complex_2_fatigue_sum_fu == 1 ~ 1,
                                               !is.na(.data$complex_2_fatigue_sum_acute) &!is.na(.data$complex_2_fatigue_sum_fu) ~ 0, 
                                               TRUE ~ NA_real_),
           complex_3_exercise_final = case_when(.data$complex_3_exercise_sum_acute == 1 & .data$complex_3_exercise_sum_fu == 1 ~ 1,
                                                !is.na(.data$complex_3_exercise_sum_acute) &!is.na(.data$complex_3_exercise_sum_fu) ~ 0, 
                                                TRUE ~ NA_real_),
           complex_4_pain_final = case_when(.data$complex_4_pain_sum_acute == 1 & .data$complex_4_pain_sum_fu == 1 ~ 1,
                                            !is.na(.data$complex_4_pain_sum_acute) &!is.na(.data$complex_4_pain_sum_fu) ~ 0, 
                                            TRUE ~ NA_real_),
           complex_5_ent_final = case_when(.data$complex_5_ent_sum_acute == 1 & .data$complex_5_ent_sum_fu == 1 ~ 1,
                                           !is.na(.data$complex_5_ent_sum_acute) &!is.na(.data$complex_5_ent_sum_fu) ~ 0, 
                                           TRUE ~ NA_real_),
           complex_6_cough_final = case_when(.data$complex_6_cough_sum_acute == 1 & .data$complex_6_cough_sum_fu == 1 ~ 1,
                                             !is.na(.data$complex_6_cough_sum_acute) &!is.na(.data$complex_6_cough_sum_fu) ~ 0, 
                                             TRUE ~ NA_real_),
           complex_7_chest_final = case_when(.data$complex_7_chest_sum_acute == 1 & .data$complex_7_chest_sum_fu == 1 ~ 1,
                                             !is.na(.data$complex_7_chest_sum_acute) &!is.na(.data$complex_7_chest_sum_fu) ~ 0, 
                                             TRUE ~ NA_real_),
           complex_8_gastro_final = case_when(.data$complex_8_gastro_sum_acute == 1 & .data$complex_8_gastro_sum_fu == 1 ~ 1,
                                              !is.na(.data$complex_8_gastro_sum_acute) &!is.na(.data$complex_8_gastro_sum_fu) ~ 0, 
                                              TRUE ~ NA_real_),
           complex_9_neuro_final = case_when(.data$complex_9_neuro_sum_acute == 1 & .data$complex_9_neuro_sum_fu == 1 ~ 1,
                                             !is.na(.data$complex_9_neuro_sum_acute) &!is.na(.data$complex_9_neuro_sum_fu) ~ 0, 
                                             TRUE ~ NA_real_),
           complex_10_derma_final = case_when(.data$complex_10_derma_sum_acute == 1 & .data$complex_10_derma_sum_fu == 1 ~ 1,
                                              !is.na(.data$complex_10_derma_sum_acute) &!is.na(.data$complex_10_derma_sum_fu) ~ 0, 
                                              TRUE ~ NA_real_),
           complex_11_flulike_final = case_when(.data$complex_11_flulike_sum_acute == 1 & .data$complex_11_flulike_sum_fu == 1 ~ 1,
                                                !is.na(.data$complex_11_flulike_sum_acute) &!is.na(.data$complex_11_flulike_sum_fu) ~ 0, 
                                                TRUE ~ NA_real_),
           complex_12_sleep_final = case_when(.data$complex_12_sleep_sum_acute == 1 & .data$complex_12_sleep_sum_fu == 1 ~ 1,
                                              !is.na(.data$complex_12_sleep_sum_acute) &!is.na(.data$complex_12_sleep_sum_fu) ~ 0, 
                                              TRUE ~ NA_real_)) %>%
    select(all_of(pid), all_of(visit_label_var_name), contains("final"))
  
  return(ecu_symptom_complex)
  
}

#' Calculate PCS Score
#' 
#' for each symptom-row in long data, apply corresponding the detect_symptom_complex and summarise long symptom data frame by PCSS symptom complex
#' returns a dataframe with one row per pid containing the results of PCS Score 
#' 
#' @param ecu_symptom_complex A dataframe containing symptom complexes
#' @importFrom rlang .data
#'
#' @return A dataframe containing PCS-Score
#' @export

calculate_pcs_score_hap <- function(ecu_symptom_complex) {
  
  ecu_pcs_score_df <- ecu_symptom_complex %>%
    mutate(pcs_score_sum_without_proms = if_else(.data$complex_1_chemo_final == "1", 3.5, 0) +
             if_else(.data$complex_2_fatigue_final == 1, 7.0, 0) +
             if_else(.data$complex_3_exercise_final == 1, 4.0, 0) +
             if_else(.data$complex_4_pain_final == 1, 6.5, 0) +
             if_else(.data$complex_5_ent_final == 1, 5.5, 0) +
             if_else(.data$complex_6_cough_final == 1, 7.0, 0) +
             if_else(.data$complex_7_chest_final == 1, 3.5, 0) +
             if_else(.data$complex_8_gastro_final == 1, 5.0, 0) +
             if_else(.data$complex_9_neuro_final == 1, 6.5, 0) +
             if_else(.data$complex_10_derma_final == 1, 2.0, 0) +
             if_else(.data$complex_11_flulike_final == 1, 3.5, 0) +
             if_else(.data$complex_12_sleep_final == 1, 5.0, 0),
           pcs_score_sum_without_proms = case_when(is.na(.data$complex_1_chemo_final) & is.na(.data$complex_2_fatigue_final) & is.na(.data$complex_3_exercise_final) &
                                                     is.na(.data$complex_4_pain_final) & is.na(.data$complex_5_ent_final) & is.na(.data$complex_6_cough_final) &
                                                     is.na(.data$complex_7_chest_final) & is.na(.data$complex_8_gastro_final) & is.na(.data$complex_9_neuro_final) &
                                                     is.na(.data$complex_10_derma_final) & is.na(.data$complex_11_flulike_final) & is.na(.data$complex_12_sleep_final) ~ NA_real_,
                                                   TRUE ~ .data$pcs_score_sum_without_proms),
           pcs_score_group_without_proms = cut(.data$pcs_score_sum_without_proms,
                                               breaks= c(-Inf, 0, 10.75, 26.25, Inf ), 
                                               labels=c("0", "<=10,75", "10,75<x<=26,25", ">26,25"))) %>%
    select(-contains("complex"))
  
  return(ecu_pcs_score_df)
  
}

#' Create data frame with Post-COVID variables without PROMs in Post-COVID-Score
#' 
#' returns a dataframe, which contains the Post-COVID variables without PROMs in Post-COVID-Score
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in data
#' @importFrom rlang .data
#' 
#' @return A dataframe with Post-COVID variables without PROMs in Post-COVID-Score
#' @export

build_hap_pcs_score_df_without_proms <- function(trial_data, pid) {
  
  table_names <- names(trial_data)
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  
  ecu_symptom_complex <- detect_symptom_complex_hap(trial_data[["ecu_long_symptom_data"]], 
                                                    vector_of_pcss_fup_visit = c("Follow-Up M3", "Follow-Up M6", "Follow-Up M12"),
                                                    pid, trial_data)
  ecu_pcs_score <- calculate_pcs_score_hap(ecu_symptom_complex) %>% 
    rename(visit_label = all_of(visit_label_var_name))

  return(ecu_pcs_score)
  
}

#' Create data frame with Symptom Complexes and Post-COVID variables with and without PROMs in Post-COVID-Score
#' 
#' returns a dataframe, which contains the symptom complexes and Post-COVID variables with and without PROMs in Post-COVID-Score
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in data
#' @importFrom rlang .data
#' 
#' @return A dataframe with symptom compelexes and Post-COVID variables with and without PROMs in Post-COVID-Score
#' @export

build_hap_pcs_score_df_with_proms <- function(trial_data, pid) {
  
  table_names <- names(trial_data)
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data[[grep("^_?visit_02$", table_names)]]), "mnpvislabel", "visit_name")
  
  ecu_symptom_complex <- detect_symptom_complex_hap(trial_data[["ecu_long_symptom_data"]], 
                                                    vector_of_pcss_fup_visit = c("Follow-Up M3", "Follow-Up M6", "Follow-Up M12"),
                                                    pid, trial_data)
  ecu_pcs_score_df <- calculate_pcs_score_hap(ecu_symptom_complex)
  
  trial_data <- primary_coding_hap_promis_29_dyspnea(trial_data)
  trial_data <- primary_coding_hap_promis_29_fatigue_4a(trial_data)
  trial_data <- primary_coding_hap_promis_cogn_funct_4a(trial_data)
  trial_data <- primary_coding_hap_promis_29_sleep_4a(trial_data)
  if (!("ecu_fss_sum" %in% names(trial_data[[grep("^_?fss$", table_names)]]))) {trial_data <- primary_coding_hap_fss(trial_data)}
  
  relevante_proms <- trial_data[[grep("^_?prom57$", table_names)]] %>%
    left_join(trial_data[[grep("^_?dysfl$", table_names)]], by = c(pid, visit_label_var_name)) %>% 
    left_join(trial_data[[grep("^_?kogfkt$", table_names)]], by = c(pid, visit_label_var_name)) %>%
    left_join(trial_data[[grep("^_?fatigue$", table_names)]] %>% filter(.data$fat_0011 == 1), by = c(pid, visit_label_var_name)) %>%
    left_join(trial_data[[grep("^_?fss$", table_names)]] %>% filter(.data$fss_0011 == 1), by = c(pid, visit_label_var_name)) %>%
    select(all_of(pid), all_of(visit_label_var_name), contains("ecu"), "fat_0022", "fat_0023", "fat_0024", "fat_0025")
  
  ecu_symptom_complex <- ecu_symptom_complex %>%
    left_join(relevante_proms, by = c(pid, visit_label_var_name)) %>%
    mutate(complex_2_fatigue_prom = case_when(.data$ecu_promis29_fatigue_4a_cat_2 == "Fatigue" | .data$ecu_fss_sum >= 36 ~ 1, 
                                              TRUE ~ .data$complex_2_fatigue_final),
           complex_3_exercise_prom = case_when(.data$ecu_promis29_dyspnea_cat_2 == "Dyspnea" | .data$fat_0022 == 1 ~ 1,
                                               TRUE ~ .data$complex_3_exercise_final),
           complex_9_neuro_prom = case_when(.data$ecu_promis_cogn_funct_4a_cat_2 == "Impairments" | .data$fat_0024 == 1 | .data$fat_0025 == 1 ~ 1,
                                            TRUE ~ .data$complex_9_neuro_final),
           complex_12_sleep_prom = case_when(.data$ecu_promis29_sleep_4a_cat_2 == "Sleep disturbance" | .data$fat_0023 == 1 ~ 1,
                                             TRUE ~ .data$complex_12_sleep_final)) 
  
  ecu_pcs_score_df <- ecu_pcs_score_df %>%
    left_join(ecu_symptom_complex, by = c(pid, visit_label_var_name)) %>%
    mutate(pcs_score_sum_with_proms = if_else(.data$complex_1_chemo_final == "1", 3.5, 0) +
             if_else(.data$complex_2_fatigue_prom == 1, 7.0, 0) +
             if_else(.data$complex_3_exercise_prom == 1, 4.0, 0) +
             if_else(.data$complex_4_pain_final == 1, 6.5, 0) +
             if_else(.data$complex_5_ent_final == 1, 5.5, 0) +
             if_else(.data$complex_6_cough_final == 1, 7.0, 0) +
             if_else(.data$complex_7_chest_final == 1, 3.5, 0) +
             if_else(.data$complex_8_gastro_final == 1, 5.0, 0) +
             if_else(.data$complex_9_neuro_prom == 1, 6.5, 0) +
             if_else(.data$complex_10_derma_final == 1, 2.0, 0) +
             if_else(.data$complex_11_flulike_final == 1, 3.5, 0) +
             if_else(.data$complex_12_sleep_prom == 1, 5.0, 0),
           pcs_score_group_with_proms = cut(.data$pcs_score_sum_with_proms,
                                            breaks= c(-Inf, 0, 10.75, 26.25, Inf ), 
                                            labels=c("0", "<=10,75", "10,75<x<=26,25", ">26,25"))) %>%
    rename(visit_label = all_of(visit_label_var_name)) %>% 
    select(all_of(pid), "visit_label", contains("pcs_score"))

  
  return(ecu_pcs_score_df)
  
}


## Helper Functions for PROMIS calculation =====================================

### PROMIS-29 Fatigue ==========================================================

#' Calculate PROMIS-29 Fatigue sum score
#' 
#' @description Calculate sum score of PROMIS-29 Fatigue
#' 
#' @param pro_fatigue_1 vector for item "I am exhausted."
#' @param pro_fatigue_2 vector for item "It is hard for me to start something because I am tired."
#' @param pro_fatigue_3 vector for item "How exhausted did you feel in general?"
#' @param pro_fatigue_4 vector for item "How exhausted were you in general?"
#' 
#' @return A numeric vector with sum score of PROMIS-29 Fatigue
#' @noRd

calculate_promis_29_fatigue_4a_sum <- function(pro_fatigue_1, pro_fatigue_2, pro_fatigue_3, pro_fatigue_4) {
  
  pro_fatigue_sum <- pro_fatigue_1 + pro_fatigue_2 + pro_fatigue_3 + pro_fatigue_4
  pro_fatigue_sum <- ifelse(pro_fatigue_1 == -1 | pro_fatigue_2 == -1 | pro_fatigue_3 == -1 | pro_fatigue_4 == -1, NA_real_, pro_fatigue_sum)
  
  return(pro_fatigue_sum)
  
}


#' Categorize PROMIS-29 Fatigue sum score in four groups
#' 
#' @description Categorize PROMIS-29 Fatigue in four groups
#' 
#' @param pro_fatigue_sum A numerical vector with PROMIS-29 Fatigue sum score
#' 
#' @return A factor /w levels "No fatigue", "Mild fatigue", "Moderate fatigue" and "Severe fatigue"
#' @noRd

categorize_promis_29_fatigue_4a <- function(pro_fatigue_sum) {
  
  pro_fatigue_cat <- case_when(pro_fatigue_sum < 11 ~ "No Fatigue",
                               pro_fatigue_sum >= 11 & pro_fatigue_sum < 14 ~ "Mild Fatigue",
                               pro_fatigue_sum >= 14 & pro_fatigue_sum < 19 ~ "Moderate Fatigue",
                               pro_fatigue_sum >= 19 ~ "Severe Fatigue")
  
  return(pro_fatigue_cat)
  
}


#' Categorize PROMIS-29 Fatigue sum score in two groups
#' 
#' @description Categorize PROMIS-29 Fatigue in two groups
#' 
#' @param pro_fatigue_sum A numerical vector with PROMIS-29 Fatigue sum score
#' 
#' @return A factor /w levels "No fatigue" and "Fatigue"
#' @noRd

categorize_promis_29_fatigue_4a_2 <- function(pro_fatigue_sum) {
  
  pro_fatigue_cat_2 <- case_when(pro_fatigue_sum < 11 ~ "No Fatigue",
                                 pro_fatigue_sum >= 11 ~ "Fatigue")
  
  return(pro_fatigue_cat_2)
  
}


#' Primary coding PROMIS-29 Fatigue
#' 
#' adds the following columns to promext:
#' ecu_promis29_fatigue_4a_sum, ecu_promis29_fatigue_4a_cat, ecu_promis29_fatigue_4a_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @noRd


primary_coding_hap_promis_29_fatigue_4a <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?prom57$", table_names)]] <- trial_data[[grep("^_?prom57$", table_names)]]  %>%
    
    mutate(ecu_promis29_fatigue_4a_sum = calculate_promis_29_fatigue_4a_sum(.data$hi7, .data$an3, .data$fatexp41, .data$fatexp40),
           ecu_promis29_fatigue_4a_cat = categorize_promis_29_fatigue_4a(.data$ecu_promis29_fatigue_4a_sum),
           ecu_promis29_fatigue_4a_cat_2 = categorize_promis_29_fatigue_4a_2(.data$ecu_promis29_fatigue_4a_sum))
  
  return(trial_data)
  
}


### PROMIS-29 Dyspnea ==========================================================

#' Count answered PROMIS-29 Dyspnea items
#' 
#' @description 
#' counts the answered PROMIS-29 Dyspnea items
#' 
#' @param pro_dysp_1 A vector for item "Dressing without assistance"
#' @param pro_dysp_2 A vector for item "Walk 50 steps on level ground at a normal pace without stopping"
#' @param pro_dysp_3 A vector for item "Walk up 20 steps (two flights of stairs) without stopping"
#' @param pro_dysp_4 A vector for item "Preparing meals"
#' @param pro_dysp_5 A vector for item "Washing dishes"
#' @param pro_dysp_6 A vector for item "Sweeping or mopping with a mop"
#' @param pro_dysp_7 A vector for item "Make a bed"
#' @param pro_dysp_8 A vector for item "Lift something that weighs 4.5 - 9 kg (e.g., a large bag of groceries)"
#' @param pro_dysp_9 A vector for item "Lifting something that weighs 4.5 - 9 kg (e.g., a large bag of groceries), carrying it from one room to another"
#' @param pro_dysp_10 A vector for item "Walk about 1 km (at a faster than your normal pace) without stopping"
#' 
#' @return A numeric vector containing the number of answered PROMIS-29 items
#' @noRd

count_n_promis_29_dyspnea <- function(pro_dysp_1, pro_dysp_2, pro_dysp_3, pro_dysp_4, pro_dysp_5, pro_dysp_6, pro_dysp_7, pro_dysp_8, pro_dysp_9,
                                      pro_dysp_10) {
  
  pro_dysp_n <- ifelse(!is.na(pro_dysp_1) & pro_dysp_1 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_2) & pro_dysp_2 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_3) & pro_dysp_3 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_4) & pro_dysp_4 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_5) & pro_dysp_5 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_6) & pro_dysp_6 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_7) & pro_dysp_7 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_8) & pro_dysp_8 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_9) & pro_dysp_9 >= 0, 1, 0) +
    ifelse(!is.na(pro_dysp_10) & pro_dysp_10 >= 0, 1, 0)
  pro_dysp_n <- ifelse(pro_dysp_n <= 0 | is.na(pro_dysp_n), NA_real_, pro_dysp_n)
  
  return(pro_dysp_n)
  
}


#' Calculate PROMIS-29 Dyspnea sum score
#' 
#' @description Calculate sum score of PROMIS-29 Dyspnea
#' 
#' @param pro_dysp_1 A vector for item "Dressing without assistance"
#' @param pro_dysp_2 A vector for item "Walk 50 steps on level ground at a normal pace without stopping"
#' @param pro_dysp_3 A vector for item "Walk up 20 steps (two flights of stairs) without stopping"
#' @param pro_dysp_4 A vector for item "Preparing meals"
#' @param pro_dysp_5 A vector for item "Washing dishes"
#' @param pro_dysp_6 A vector for item "Sweeping or mopping with a mop"
#' @param pro_dysp_7 A vector for item "Make a bed"
#' @param pro_dysp_8 A vector for item "Lift something that weighs 4.5 - 9 kg (e.g., a large bag of groceries)"
#' @param pro_dysp_9 A vector for item "Lifting something that weighs 4.5 - 9 kg (e.g., a large bag of groceries), carrying it from one room to another"
#' @param pro_dysp_10 A vector for item "Walk about 1 km (at a faster than your normal pace) without stopping"
#' 
#' @return A numeric vector with sum score of PROMIS-29 Dyspnea
#' @noRd

calculate_promis_29_dyspnea_sum <- function(pro_dysp_1, pro_dysp_2, pro_dysp_3, pro_dysp_4, pro_dysp_5, pro_dysp_6, pro_dysp_7, pro_dysp_8, pro_dysp_9,
                                            pro_dysp_10, pro_dysp_n) {
  
  pro_dysp_sum <- ifelse(is.na(pro_dysp_1) | pro_dysp_1 < 0, 0, pro_dysp_1) +
    ifelse(is.na(pro_dysp_2) | pro_dysp_2 < 0, 0, pro_dysp_2) +
    ifelse(is.na(pro_dysp_3) | pro_dysp_3 < 0, 0, pro_dysp_3) +
    ifelse(is.na(pro_dysp_4) | pro_dysp_4 < 0, 0, pro_dysp_4) + 
    ifelse(is.na(pro_dysp_5) | pro_dysp_5 < 0, 0, pro_dysp_5) +
    ifelse(is.na(pro_dysp_6) | pro_dysp_6 < 0, 0, pro_dysp_6) + 
    ifelse(is.na(pro_dysp_7) | pro_dysp_7 < 0, 0, pro_dysp_7) +
    ifelse(is.na(pro_dysp_8) | pro_dysp_8 < 0, 0, pro_dysp_8) +
    ifelse(is.na(pro_dysp_9) | pro_dysp_9 < 0, 0, pro_dysp_9) + 
    ifelse(is.na(pro_dysp_10) | pro_dysp_10 < 0, 0, pro_dysp_10)
  pro_dysp_sum <- ifelse(pro_dysp_n >= 4, pro_dysp_sum, NA_real_)
  
  return(pro_dysp_sum)
  
}


#' Categorize PROMIS-29 Dyspnea sum score in four groups
#' 
#' @description Categorize PROMIS-29 Dyspnea in four groups
#' 
#' @param pro_dysp_sum A numerical vector with PROMIS-29 Dyspnea sum score
#' 
#' @return A factor /w levels "No dyspnea", "Mild dyspnea", "Moderate dyspnea" and "Severe dyspnea"
#' @noRd

categorize_promis_29_dyspnea <- function(pro_dysp_sum) {
  
  pro_dysp_cat <- case_when(pro_dysp_sum < 15 ~ "No Dyspnea",
                            pro_dysp_sum >= 15 & pro_dysp_sum < 20 ~ "Mild Dyspnea",
                            pro_dysp_sum >= 20 & pro_dysp_sum < 28 ~ "Moderate Dyspnea",
                            pro_dysp_sum >= 28 ~ "Severe Dyspnea")
  
  return(pro_dysp_cat)
  
}


#' Categorize PROMIS-29 Dyspnea sum score in two groups
#' 
#' @description Categorize PROMIS-29 Dyspnea in two groups
#' 
#' @param pro_dysp_sum A numerical vector with PROMIS-29 Dyspnea sum score
#' 
#' @return A factor /w levels "No dyspnea" and "Dyspnea"
#' @noRd

categorize_promis_29_dyspnea_2 <- function(pro_dysp_sum) {
  
  pro_dysp_cat_2 <- case_when(pro_dysp_sum < 15 ~ "No Dyspnea",
                              pro_dysp_sum >= 15 ~ "Dyspnea")
  
  return(pro_dysp_cat_2)
  
}


#' Primary coding PROMIS-29 Dyspnea
#' 
#' adds the following columns to promext:
#' ecu_promis29_dyspnea_sum, ecu_promis29_dyspnea_cat, ecu_promis29_dyspnea_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @noRd

primary_coding_hap_promis_29_dyspnea <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?dysfl$", table_names)]] <- trial_data[[grep("^_?dysfl$", table_names)]]  %>%
    
    mutate(ecu_promis29_dyspnea_n = count_n_promis_29_dyspnea(.data$dysfl001, .data$dysfl002, .data$dysfl003, .data$dysfl004, .data$dysfl005, .data$dysfl006,
                                                              .data$dysfl007, .data$dysfl008, .data$dysfl009, .data$dysfl010),
           ecu_promis29_dyspnea_sum = calculate_promis_29_dyspnea_sum(.data$dysfl001, .data$dysfl002, .data$dysfl003, .data$dysfl004, .data$dysfl005, .data$dysfl006,
                                                                      .data$dysfl007, .data$dysfl008, .data$dysfl009, .data$dysfl010, .data$ecu_promis29_dyspnea_n),
           ecu_promis29_dyspnea_cat = categorize_promis_29_dyspnea(.data$ecu_promis29_dyspnea_sum),
           ecu_promis29_dyspnea_cat_2 = categorize_promis_29_dyspnea_2(.data$ecu_promis29_dyspnea_sum))
  
  return(trial_data)
  
}


### PROMIS Cognitive Impairments ===============================================

#' Calculate PROMIS-29 Cognitive impairments sum score
#' 
#' @description Calculate sum score of PROMIS-29 Cognitive impairments
#' 
#' @param pro_cogn_1 A vector for item "I was slow in thinking."
#' @param pro_cogn_2 A vector for item "It seemed to me that my brain was not working as well as usual."
#' @param pro_cogn_3 A vector for item "I had to make a greater effort than usual to keep track of what I was busy with."
#' @param pro_cogn_4 A vector for item "I had difficulty switching back and forth between different activities that required some thinking."
#' 
#' @return A numeric vector with sum score of PROMIS-29 Cognitive impairments
#' @noRd

calculate_promis_cognitive_funct_4a_sum <- function(pro_cogn_1, pro_cogn_2, pro_cogn_3, pro_cogn_4) {
  
  pro_cogn_sum <- pro_cogn_1 + pro_cogn_2 + pro_cogn_3 + pro_cogn_4
  
  return(pro_cogn_sum)
  
}


#' Categorize PROMIS-29 Cognitive impairments sum score in four groups
#' 
#' @description Categorize PROMIS-29 Cognitive impairments in four groups
#' 
#' @param pro_dysp_sum A numerical vector with PROMIS-29 Cognitive impairments sum score
#' 
#' @return A factor /w levels "No cognitive impairments", "Mild cognitive impairments", "Moderate cognitive impairments" and "Severe cognitive impairments"
#' @noRd

categorize_promis_cognitive_funct_4a <- function(pro_cogn_sum) {
  
  pro_cogn_cat <- case_when(pro_cogn_sum <= 5 ~ "Severe cognitive impairment",
                            pro_cogn_sum >= 6 & pro_cogn_sum <= 11 ~ "Moderate cognitive impairment",
                            pro_cogn_sum >= 12 & pro_cogn_sum <= 14 ~ "Mild cognitive impairment",
                            pro_cogn_sum >= 15 ~ "No cognitive impairments")
  
  return(pro_cogn_cat)
  
}


#' Categorize PROMIS-29 Cognitive impairments sum score in two groups
#' 
#' @description Categorize PROMIS-29 Cognitive impairments in two groups
#' 
#' @param pro_dysp_sum A numerical vector with PROMIS-29 Cognitive impairments sum score
#' 
#' @return A factor /w levels "No cognitive impairments" and "Cognitive impairments"
#' @noRd

categorize_promis_cognitive_funct_4a_2 <- function(pro_cogn_sum) {
  
  pro_cogn_cat_2 <- case_when(pro_cogn_sum < 15 ~ "Cognitive impairments",
                              pro_cogn_sum >= 15 ~ "No cognitive impairments")
  
  return(pro_cogn_cat_2)
  
}


#' Primary coding PROMIS-29 Cognitive impairments
#' 
#' adds the following columns to prom:
#' ecu_promis_cogn_funct_4a_sum, ecu_promis_cogn_funct_4a_cat, ecu_promis_cogn_funct_4a_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @noRd

primary_coding_hap_promis_cogn_funct_4a <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?kogfkt$", table_names)]] <- trial_data[[grep("^_?kogfkt$", table_names)]]  %>%
    
    mutate(ecu_promis_cogn_funct_4a_sum = calculate_promis_cognitive_funct_4a_sum(.data$pc2r, .data$pc35r, .data$pc36r, .data$pc42r),
           ecu_promis_cogn_funct_4a_cat = categorize_promis_cognitive_funct_4a(.data$ecu_promis_cogn_funct_4a_sum),
           ecu_promis_cogn_funct_4a_cat_2 = categorize_promis_cognitive_funct_4a_2(.data$ecu_promis_cogn_funct_4a_sum))
  
  return(trial_data)
  
}

### PROMIS Sleep disturbance ===================================================

#' Calculate PROMIS-29 Sleep disturbance sum score
#' 
#' @description Calculate sum score of PROMIS-29 Sleep disturbance
#' 
#' @param pro_sleep_1 A vector for item "My sleep quality was"
#' @param pro_sleep_2 A vector for item "My sleep was refreshing."
#' @param pro_sleep_3 A vector for item "I had trouble sleeping."
#' @param pro_sleep_4 A vector for item "I had difficulty falling asleep."
#' 
#' @return A numeric vector with sum score of PROMIS-29 Sleep disturbance
#' @noRd

calculate_promis_29_sleep_4a_sum <- function(pro_sleep_1, pro_sleep_2, pro_sleep_3, pro_sleep_4) {
  
  pro_sleep_sum <- pro_sleep_1 + pro_sleep_2 + pro_sleep_3 + pro_sleep_4
  
  return(pro_sleep_sum)
  
}


#' Categorize PROMIS-29 Sleep disturbance sum score in four groups
#' 
#' @description Categorize PROMIS-29 Sleep disturbance in four groups
#' 
#' @param pro_sleep_sum A numerical vector with PROMIS-29 Sleep disturbance sum score
#' 
#' @return A factor /w levels "No sleep disturbance", "Mild sleep disturbance", "Moderate sleep disturbance" and "Severe sleep disturbance"
#' @noRd

categorize_promis_29_sleep_4a <- function(pro_sleep_sum) {
  
  pro_sleep_cat <- case_when(pro_sleep_sum < 13 ~ "No sleep disturbance",
                             pro_sleep_sum >= 13 & pro_sleep_sum <= 15 ~ "Mild sleep disturbance",
                             pro_sleep_sum >= 16 & pro_sleep_sum <= 19 ~ "Moderate sleep disturbance",
                             pro_sleep_sum >= 20 ~ "Severe sleep disturbance")
  
  return(pro_sleep_cat)
  
}


#' Categorize PROMIS-29 Sleep disturbance sum score in two groups
#' 
#' @description Categorize PROMIS-29 Sleep disturbance in two groups
#' 
#' @param pro_dysp_sum A numerical vector with PROMIS-29 Sleep disturbance sum score
#' 
#' @return A factor /w levels "No sleep disturbance" and "Sleep disturbance"
#' @noRd

categorize_promis_29_sleep_4a_2 <- function(pro_sleep_sum) {
  
  pro_sleep_cat_2 <- case_when(pro_sleep_sum < 13 ~ "No sleep disturbance",
                               pro_sleep_sum >= 12 ~ "Sleep disturbance")
  
  return(pro_sleep_cat_2)
  
}


#' Primary coding PROMIS-29 Sleep disturbance
#' 
#' adds the following columns to promext:
#' ecu_promis29_sleep_4a_sum, ecu_promis29_sleep_4a_cat, ecu_promis29_sleep_4a_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @noRd

primary_coding_hap_promis_29_sleep_4a <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?prom57$", table_names)]] <- trial_data[[grep("^_?prom57$", table_names)]]  %>%
    
    mutate(ecu_promis29_sleep_4a_sum = calculate_promis_29_sleep_4a_sum(.data$sleep109, .data$sleep116, .data$sleep20, .data$sleep44),
           ecu_promis29_sleep_4a_cat = categorize_promis_29_sleep_4a(.data$ecu_promis29_sleep_4a_sum),
           ecu_promis29_sleep_4a_cat_2 = categorize_promis_29_sleep_4a_2(.data$ecu_promis29_sleep_4a_sum))
  
  return(trial_data)
  
}