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
# in the last step of this script, in the function primary_coding_rapid_revive(), all selected primary_coding_rapid_revive steps are performed consecutively. 
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
    # birth date was only reported as year (YYYY), but needed to be YYYY-MM-DD --> we added -06-30 to set the birthdate to July 1st as middle of the respective year
    dplyr::mutate(ecu_demo_birth_new = ymd(paste0(.data$demo_birth, "-06-30")), 
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


## Heart frequency ==============================================================

#' Primary coding heart frequency
#'
#' adds the following columns to vital: 
#' ecu_hr
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_rapid_revive_hf <- function(trial_data) {
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?vital$", table_names)]] <- trial_data[[grep("^_?vital$", table_names)]] %>%
    dplyr::mutate(ecu_hf = categorize_heartfrequency_ecu(.data$vital_freq))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_hf = ""
  )
  
  return (trial_data)
}


## Respiration rate sensor-based measurement (night) ==============================================================

#' Primary coding respiration rate
#'
#' adds the following columns to vital: 
#' ecu_rr
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_rapid_revive_resp_rate <- function(trial_data) {
  table_names <- names(trial_data)
  trial_data[[grep("^_?eohrsub$", table_names)]] <- trial_data[[grep("^_?eohrsub$", table_names)]] %>%
    dplyr::mutate(ecu_os_resp_rate = categorize_resp_rate_ecu(.data$os_nocrr))
  
  labelled::var_label(trial_data[[grep("^_?eohrsub$", table_names)]]) <- list(
    ecu_os_resp_rate = ""
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

## Generalized Anxiety Disorder 7 (GAD-7) ======================================

#' Primary coding Generalized Anxiety Disorder 7 (GAD-7)
#' 
#' adds the following columns to gad7:
#' ecu_gad7_sum
#' ecu_gad_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export


primary_coding_rapid_revive_gad7 <- function(trial_data) {
  
  trial_data[["gad7"]] <- trial_data[["gad7"]] %>%
    mutate(ecu_gad7_sum = calculate_gad7_sum(.data$gad7_001, .data$gad7_002, .data$gad7_003, .data$gad7_004, .data$gad7_005, .data$gad7_006, .data$gad7_007),
           ecu_gad7_cat = categorize_gad7_ecu(.data$ecu_gad7_sum))
  
  labelled::var_label(trial_data[["gad7"]]) <- list(
    ecu_gad7_sum = "",
    ecu_gad7_cat = ""
  )
  
  return(trial_data)
}


## modified Medical Research Council Dyspnea Scale (mMRC) ======================

#' Primary coding modified Medical Research Council Dyspnea Scale (mMRC)
#' 
#' adds the following column to mmrc: 
#' ecu_mmrc
#'
#' @param trial_data A secuTrial data object
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_mmrc <- function(trial_data) {
  
  trial_data[["mmrc"]] <- trial_data[["mmrc"]] %>%
    mutate(ecu_mmrc = case_when(
      .data$mmrc_sym == 0 ~ "No Dyspnea",
      .data$mmrc_sym > 0 ~ "Dyspnea",
      TRUE ~ NA
    ))
              #categorize_mmrc_ecu(.data$mmrc_sym.factor)) 
  
  labelled::var_label(trial_data[["mmrc"]]) <- list(
    ecu_mmrc = ""
  )
  
  return(trial_data)
}


## Montreal Cognitive Assessment (MoCA) ========================================

#' Primary cpding for Montreal Cognitive Assessment (MoCA)
#' 
#' adds the following varible to moca
#' ecu_moca_total_score, ecu_moca_cat
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_moca <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?moca$", table_names)]] <- trial_data[[grep("^_?moca$", table_names)]] %>%
    mutate(ecu_moca_a_7_sum = .data$moca_a_7_93 + .data$moca_a_7_86 + .data$moca_a_7_79 + .data$moca_a_7_72 + .data$moca_a_7_65, 
           ecu_moca_total_score = .data$moca_v_atm + 
             .data$moca_v_cube + 
             .data$moca_v_clock_con + .data$moca_v_clock_num + .data$moca_v_clock_han + 
             .data$moca_n_lion + .data$moca_n_rhi + .data$moca_n_cam +
             .data$moca_a_num_f + .data$moca_a_num_b +
             .data$moca_a_let_a + 
             ifelse(.data$ecu_moca_a_7_sum == 0, 0, 
                    ifelse(.data$ecu_moca_a_7_sum == 1, 1, 
                           ifelse(.data$ecu_moca_a_7_sum == 2 | .data$ecu_moca_a_7_sum == 3, 2, 
                                  ifelse(.data$ecu_moca_a_7_sum >= 4, 3, NA)))) +
             .data$moca_s_rep1 + .data$moca_s_rep2 + 
             .data$moca_s_let_f +
             .data$moca_ab_tran + .data$moca_ab_meas + 
             .data$moca_m_face3 + .data$moca_m_vel3 + .data$moca_m_chur3 + .data$moca_m_tul3 + .data$moca_m_red3 +
             .data$moca_o_date + .data$moca_o_mon + .data$moca_o_year + .data$moca_o_day + .data$moca_o_loc + .data$moca_o_city + 
             .data$moca_ed_12,
           ecu_moca_cat = categorize_moca_ecu(.data$ecu_moca_total_score)) 
  
  labelled::var_label(trial_data[[grep("^_?moca$", table_names)]]) <- list(
    ecu_moca_total_score = "",
    ecu_moca_cat = ""
  )
  
  return(trial_data)
}


## Patient Health Questionnaire Depession Scale (PHQ-9) ========================

#' Primary coding Patient Health Questionnaire Depession Scale (PHQ-9)
#' 
#' adds the following column to phq9: 
#' ecu_phq9_sum, ecu_phq9_cat, ecu_phq9_cat_2
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_phq9 <- function(trial_data) {
  
  trial_data[["phq9"]] <- trial_data[["phq9"]] %>%
    dplyr::mutate(ecu_phq9_sum = calculate_phq9_sum(.data$phq9_001, .data$phq9_002, .data$phq9_003, .data$phq9_004, .data$phq9_005, 
                                                    .data$phq9_006, .data$phq9_007, .data$phq9_008, .data$phq9_009),
                  ecu_phq9_cat = categorize_phq9_ecu(.data$ecu_phq9_sum),
                  ecu_phq9_cat_2 = categorize_phq9_ecu_2(.data$ecu_phq9_sum))
  
  labelled::var_label(trial_data[["phq9"]]) <- list(
    ecu_phq9_sum = "",
    ecu_phq9_cat = "",
    ecu_phq9_cat_2 = ""
  )
  
  return(trial_data)
}


## Patient Health Questionnaire 15-Item Somatic Symptom Severity Scale (PHQ15) ======

#' Primary coding Patient Health Questionnaire 15-Item Somatic Symptom Severity Scale (PHQ15)
#' 
#' adds the following column to phq15: 
#' ecu_phq15_sum, ecu_phq15_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_phq15 <- function(trial_data) {
  
  trial_data[["phq15"]] <- trial_data[["phq15"]] %>%
    dplyr::mutate(ecu_phq15_sum = calculate_phq15_sum(.data$phq15_001, .data$phq15_002, .data$phq15_003, .data$phq15_004, .data$phq15_005, 
                                                    .data$phq15_006, .data$phq15_007, .data$phq15_008, .data$phq15_009, .data$phq15_010,
                                                    .data$phq15_011, .data$phq15_012, .data$phq15_013, .data$phq15_014, .data$phq15_015),
                  ecu_phq15_cat = categorize_phq15_ecu(.data$ecu_phq15_sum))
  
  labelled::var_label(trial_data[["phq15"]]) <- list(
    ecu_phq15_sum = "",
    ecu_phq15_cat = ""
  )
  
  return(trial_data)
}


## Patient Health Questionnaire Stress Scale (PHQ-Stress) ========================

#' Primary coding Patient Health Questionnaire Stress Scale (PHQ-Stress)
#' 
#' adds the following column to phqs: 
#' ecu_phqs_sum, ecu_phqs_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_rapid_revive_phqs <- function(trial_data) {
  
  trial_data[["phqs"]] <- trial_data[["phqs"]] %>%
    dplyr::mutate(ecu_phqs_sum = calculate_phqs_sum(.data$phqs_001, .data$phqs_002, .data$phqs_003, .data$phqs_004, .data$phqs_005, 
                                                    .data$phqs_006, .data$phqs_007, .data$phqs_008, .data$phqs_009, .data$phqs_010),
                  ecu_phqs_cat = categorize_phqs_ecu(.data$ecu_phqs_sum))
  
  labelled::var_label(trial_data[["phqs"]]) <- list(
    ecu_phqs_sum = "",
    ecu_phqs_cat = ""
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
  
  ## Heart frequency ============================================================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_hf(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_hf() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Respiration rate ============================================================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_resp_rate(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_resp_rate() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Fatigue Severity Scale (FSS) ==============================================
    tryCatch(expr = {trial_data <- primary_coding_rapid_revive_fss(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_fss() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Montreal Cognitive Assessment (MoCA) ======================================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_moca(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_moca() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Patient Health Questionnaire Depession Scale (PHQ-9) ======================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_phq9(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_phq9() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Patient Health Questionnaire 15-Item Somatic Symptom Severity Scale (PHQ15) ======
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_phq15(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_phq15() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Patient Health Questionnaire Stress Scale (PHQ-Stress) ======================
  tryCatch(expr = {trial_data <- primary_coding_rapid_revive_phqs(trial_data)},
           error = function(e) {
             warning("primary_coding_rapid_revive_phqs() did not work. This is likely due to missing variables.")
             print(e)})
  
  catw("Primary Coding done")
  
  return(trial_data)
}

