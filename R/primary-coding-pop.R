#################
# Primary coding for NAPKON POP secuTrial data via secuTrialR
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

# structure of functions with the prefix primary_coding_pop 
# The functions create one or more new columns (using mutate)
# the functions return the trial data including new columns
# in the last step of this script, in the function primary_coding_pop(), all selected primary_coding_pop steps are performed consecutively. 
# this is the function which will be called in the run script to create trial_data from trial_data_raw


# Demographics =================================================================

## Age =========================================================================

#' Primary coding age
#'
#' adds the following columns to erstbefragung: 
#' ecu_age - age in years, ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_age <- function(trial_data) {
  
  trial_data[["erstbefragung"]] <- trial_data[["erstbefragung"]] %>%
    mutate(ecu_age_cat_dec = ecu_age_cat_dec(.data$gec_demo_age),
           ecu_age_cat_3 = ecu_age_cat_3(.data$gec_demo_age))
  
  return(trial_data)
}


## Migration background =========================================================================

#' Primary migration background
#'
#' adds the following columns to erstbefragung: 
#' ecu_migration_background 
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_migration <- function(trial_data) {
  
  trial_data[["erstbefragung"]] <- trial_data[["erstbefragung"]] %>%
    mutate(ecu_migration_background = get_ecu_background_pop(.data$staatsang, .data$gebland, .data$gebland2))
  
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

primary_coding_pop_bmi <- function(trial_data) {
  
  trial_data[["anthropo"]] <- trial_data[["anthropo"]] %>%
    mutate(ecu_bmi = calculate_bmi(.data$gec_weight, .data$gec_height),
           ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi, .data$gec_height_unk.factor, .data$gec_weight_unk.factor),
           ecu_bmi_adipositas = case_when(!is.na(.data$ecu_bmi_cat) ~  fct_collapse(.data$ecu_bmi_cat,
                                                                                    Ja = c("Adipositas Grad I", "Adipositas Grad II", "Adipositas Grad III"),
                                                                                    Nein = c("Untergewicht", "Normalgewicht", "\u00dcbergewicht"))))
  
  return (trial_data)
}


## Abdominal overweight ========================================================

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to anthropo: 
#' ecu_waist
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_pop_abd_overw <- function(trial_data, pid) {
  
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$erstbefragung), "mnpvislabel", "visit_name")
  
  needed_vars <- trial_data[["anthropo"]] %>%
    mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")) %>%
    select(pid, .data$visit_name_temp) %>%
    left_join(trial_data[["erstbefragung"]] %>% select (pid, .data$gec_gender, .data$gec_gender.factor, visit_label_var_name) %>%
                mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")), by = c(pid, "visit_name_temp")) %>%
    select(-.data$visit_name)
  
  trial_data[["anthropo"]] <- trial_data[["anthropo"]] %>%
    mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")) %>%
    left_join(needed_vars, by = c(pid, "visit_name_temp")) %>%
    mutate(ecu_waist = case_when(.data$gec_gender.factor == "Weiblich" & .data$taillumfang <= 88 ~ "No abdominal overweight",
                                 .data$gec_gender.factor == "Weiblich" & .data$taillumfang > 88 ~ "Abdominal overweight",
                                 .data$gec_gender.factor == "M\u00e4nnlich" & .data$taillumfang <= 102 ~ "No abdominal overweight",
                                 .data$gec_gender.factor == "M\u00e4nnlich" & .data$taillumfang > 102 ~ "Abdominal overweight")) %>%
    select(-contains("gec_gender"), -.data$visit_name_temp)
  
  return(trial_data)
}


## Clinical Parameters =========================================================================

#' Primary coding Clinical Parameters
#'
#' adds the following columns to anthropo: 
#' ecu_temp, ecu_sdp, ecu_dbp, ecu_bp
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_clinical_params <- function(trial_data) {
  
  trial_data[["anthropo"]] <- trial_data[["anthropo"]] %>%
    mutate(ecu_vitals_temp = (.data$gec_vitals_temp + .data$gec_vitals_temp_2)/2,
           ecu_vitals_temp = coalesce(.data$ecu_vitals_temp, .data$gec_vitals_temp),
           ecu_temp = categorize_temp_ecu(.data$ecu_vitals_temp),
           ecu_sbp = (.data$gec_vitals_psys + .data$gec_vitals_psys_2)/2,
           ecu_sbp = coalesce(.data$ecu_sbp, .data$gec_vitals_psys),
           ecu_dbp = (.data$gec_vitals_pdias + .data$gec_vitals_pdias_2)/2,
           ecu_dbp = coalesce(.data$ecu_dbp, .data$gec_vitals_pdias),
           ecu_bp = categorize_bloodpressure_ecu(.data$ecu_sbp, .data$ecu_dbp),
           ecu_resp_rate = categorize_resp_rate_ecu(.data$gec_vitals_resp)
    )
  
  return (trial_data)
}

## Cardiological Parameters =============================================================

#' Primary coding cardiological parameters
#' 
#' adds the following columns to kardio: 
#' ecu_bpm,
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_cardio_params <- function(trial_data) {
  
  trial_data[["kardio"]] <- trial_data[["kardio"]] %>%
    mutate(ecu_bpm = categorize_heartfrequency_ecu(.data$gec_vitals_hf))
  
  return(trial_data)
}


## Pneumological Parameters =============================================================

#' Primary coding cardiological parameters
#' 
#' adds the following columns to kardio: 
#' ecu_spo2,
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_pneumo_params <- function(trial_data) {
  
  trial_data[["pneumo"]] <- trial_data[["pneumo"]] %>%
    mutate(ecu_spo2 = categorize_oxigensaturation_ecu(.data$gec_vitals_so2))
  
  return(trial_data)
}


# Scores =======================================================================

# the following scores are categorized according to primary coding:
# MoCa, EQ5D-5L, mMRC, PHQ-8, GAD-7, GPAQ, FACIT-F, BRS, WHO-Scale, PCS-Score

# ============================================================================ #


#' Primary coding EQ5D-5L-Index
#' 
#' adds the following column to erstbefragung: 
#' ecu_eq5d_index
#'
#' @param trial_data A secuTrial data object
#' @import eq5d
#' @importFrom rlang .data
#' @export

primary_coding_pop_eq5d5l <- function(trial_data) {
  
  trial_data[["erstbefragung"]] <- trial_data[["erstbefragung"]] %>%
    mutate (ecu_eq5d5l_index = calculate_eq5d5l_index(.data$eq5d5l1, .data$eq5d5l2, .data$eq5d5l3, .data$eq5d5l4, .data$eq5d5l5))
  
  return(trial_data)
}


#' Primary coding Montreal Cognitive Assessment (MoCA) 
#' 
#' adds the following column to neuro
#' ecu_moca_total_score, ecu_moca_cat
#' 
#' @param trial_data A secuTrial data object
#' @export

primary_coding_pop_moca <- function(trial_data) {
  
  trial_data[["neuro"]] <- trial_data[["neuro"]] %>%
    mutate(ecu_moca_total_score = .data$moca_tmt + .data$moca_figure + .data$moca_clock + .data$moca_naming + .data$moca_attention + .data$moca_speech + 
             .data$moca_abstract + .data$moca_recall_number + .data$moca_orientation + .data$moca_education,
           ecu_moca_cat = categorize_moca_ecu(.data$ecu_moca_total_score)
    ) 
  
  return(trial_data)
}


#' Primary coding modified Medical Research Council Dyspnea Scale (mMRC)
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_mmrc
#'
#' @param trial_data A secuTrial data object
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_pop_mmrc <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate (ecu_mmrc = categorize_mmrc_ecu(.data$mmrc_grad.factor)) 
  
  return(trial_data)
}


#' Primary coding Patient Health Questionnaire depession scale (PHQ-8)
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_phq8
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_phq8 <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_phq8_sum = calculate_phq8_sum(.data$phq8_1, .data$phq8_2, .data$phq8_3, .data$phq8_4, .data$phq8_5, .data$phq8_6, .data$phq8_7, .data$phq8_8),
           ecu_phq8_cat = categorize_phq8_ecu(.data$ecu_phq8_sum))
  
  return(trial_data)
}


#' Primary coding Generalized Anxiety Disorder 7 (GAD-7)
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_gad7
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export


primary_coding_pop_gad7 <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_gad7_sum = calculate_gad7_sum(.data$gad7_1, .data$gad7_2, .data$gad7_3, .data$gad7_4, .data$gad7_5, .data$gad7_6, .data$gad7_7),
           ecu_gad7_cat = categorize_gad7_ecu(.data$ecu_gad7_sum))
  
  return(trial_data)
}


#' Primary coding Functional Assessment of Chronic Illness Therapy - Fatigue (FACIT-Fatigue Scale)
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_facitf
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_facitf <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_facitf_sum = calculate_facitf_sum(.data$facitf1, .data$facitf2, .data$facitf3, .data$facitf4, .data$facitf5, .data$facitf6, .data$facitf7, 
                                                 .data$facitf8, .data$facitf9, .data$facitf10, .data$facitf11, .data$facitf12, .data$facitf13),
           ecu_facitf_cat = categorize_facitf_ecu(.data$ecu_facitf_sum)) 
  
  return(trial_data)
}


#' Primary coding Brief Resilience Scale (BRS)
#' 
#' adds the following columns to surveyfrageboge: 
#' ecu_brs_sum, ecu_brs_n, ecu_brs_total, ecu_brs_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_brs <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_brs_sum = calculate_brs_sum(.data$brs1, .data$brs2, .data$brs3, .data$brs4, .data$brs5, .data$brs6),
           ecu_brs_n = calculate_brs_n(.data$brs1, .data$brs2, .data$brs3, .data$brs4, .data$brs5, .data$brs6),
           ecu_brs_total = calculate_brs_total(.data$ecu_brs_sum, .data$ecu_brs_n),
           ecu_brs_cat = categorize_brs_ecu(.data$ecu_brs_total)) 
  
  return(trial_data)
}


#' Pittsburgh Sleep Quality Index (PSQI)
#' 
#' add the following columns to surveyfrageboge:
#' ecu_psqi_comp_1, ecu_psqi_comp_2_sum, ecu_psqi_comp_2, ecu_psqi_comp_3, ecu_psqi_comp_4, ecu_psqi_comp_5_sum, ecu_psqi_comp_5, ecu_psqi_comp_6, 
#' ecu_psqi_comp_7_sum, ecu_psqi_comp_7, ecu_psqi_global_score
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_psqi <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_psqi_comp_1 = .data$psqi6,
           ecu_psqi_comp_2_sum = case_when(.data$psqi2 <= 15 ~ 0,
                                           .data$psqi2 >= 16 & .data$psqi2 <= 30 ~ 1,
                                           .data$psqi2 >= 31 & .data$psqi2 <= 60 ~ 2,
                                           .data$psqi2 > 60 ~ 3) +
             .data$psqi5a,
           ecu_psqi_comp_2 = case_when(.data$ecu_psqi_comp_2_sum == 0 ~ 0,
                                       .data$ecu_psqi_comp_2_sum == 1 | .data$ecu_psqi_comp_2_sum == 2 ~ 1,
                                       .data$ecu_psqi_comp_2_sum == 3 | .data$ecu_psqi_comp_2_sum == 4 ~ 2,
                                       .data$ecu_psqi_comp_2_sum >= 5 ~ 3),
           ecu_psqi_comp_3 = case_when(.data$psqi4 > 7 ~ 0, # > 7 hours
                                       .data$psqi4 >= 6 & .data$psqi4 <= 7 ~ 1, # 6-7 hours
                                       .data$psqi4 >= 5 & .data$psqi4 < 6 ~ 2, # 5-6 hours
                                       .data$psqi4 < 5 ~ 3), # < 5 hours
           ecu_psqi_comp_4 = case_when(.data$psqi4/(.data$psqi3 - .data$psqi1) * 100 >= 85 ~ 0,
                                       .data$psqi4/(.data$psqi3 - .data$psqi1) * 100 >= 75 & .data$psqi4/(.data$psqi3 - .data$psqi1) * 100 <= 84 ~ 1,
                                       .data$psqi4/(.data$psqi3 - .data$psqi1) * 100 >= 65 & .data$psqi4/(.data$psqi3 - .data$psqi1) * 100 <= 74 ~ 2,
                                       .data$psqi4/(.data$psqi3 - .data$psqi1) * 100 <= 64 ~ 3),
           ecu_psqi_comp_5_sum = .data$psqi5b + .data$psqi5c + .data$psqi5d + .data$psqi5e + .data$psqi5f + .data$psqi5g + .data$psqi5h + .data$psqi5i + .data$psqi5j,
           ecu_psqi_comp_5 = case_when(.data$ecu_psqi_comp_5_sum == 0 ~ 0,
                                       .data$ecu_psqi_comp_5_sum >= 1 & .data$ecu_psqi_comp_5_sum <= 9 ~ 1,
                                       .data$ecu_psqi_comp_5_sum >= 10 & .data$ecu_psqi_comp_5_sum <= 18 ~ 2,
                                       .data$ecu_psqi_comp_5_sum >= 19 ~ 3),
           ecu_psqi_comp_6 = .data$psqi7,
           ecu_psqi_comp_7_sum = .data$psqi8 + .data$psqi9,
           ecu_psqi_comp_7 = case_when(.data$ecu_psqi_comp_7_sum == 0 ~ 0,
                                       .data$ecu_psqi_comp_7_sum == 1 | .data$ecu_psqi_comp_7_sum == 2 ~ 1,
                                       .data$ecu_psqi_comp_7_sum == 3 | .data$ecu_psqi_comp_7_sum == 4 ~ 2,
                                       .data$ecu_psqi_comp_7_sum >= 5 ~ 3),
           ecu_psqi_global_score = .data$ecu_psqi_comp_1 + .data$ecu_psqi_comp_2 + .data$ecu_psqi_comp_3 + .data$ecu_psqi_comp_4 + .data$ecu_psqi_comp_5 + 
             .data$ecu_psqi_comp_6 + .data$ecu_psqi_comp_7) 
  
  return(trial_data)
}


#' Kansas City Cardiomyopathy Questionnaire (KCCQ)
#'
#' adds the following variables to surveyfrageboge:
#' ecu_kccq_phys_sum, ecu_kccq_phys_score, ecu_kccq_sy_ch, ecu_kccq_sy_ch_score, ecu_kccq_3_score,
#' ecu_kccq_5_score, ecu_kccq_7_score, ecu_kccq_9_score, ecu_kccy_sy_freq_score, ecu_kccq_sy_sev_sum,
#' ecu_kccq_sy_sev_score, ecu_kccq_sy_sev_sum, ecu_kccq_sy_sev_score, ecu_kccq_sy_sum, ecu_kccq_sy_score,
#' ecu_kccq_se_sum, ecu_kccq_se_score, ecu_kccq_qol_sum, ecu_kccq_qol_score, ecu_kccq_sl_sum,
#' ecu_kccq_sl_score, ecu_kccq_total
#' 
#' @description calculates Kansas City Cardiomyopathy Questionnaire (KCCQ) total score
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_kccq <- function(trial_data){
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    rowwise() %>%
    mutate(
      # build score for physical limitations (questions 1a to 1f)
      ## recode when question can not be answered
      kccq_1_1 = ifelse(.data$kccq_1_1 == 9, NA_real_, .data$kccq_1_1),
      kccq_1_2 = ifelse(.data$kccq_1_2 == 9, NA_real_, .data$kccq_1_2),
      kccq_1_3 = ifelse(.data$kccq_1_3 == 9, NA_real_, .data$kccq_1_3),
      kccq_1_4 = ifelse(.data$kccq_1_4 == 9, NA_real_, .data$kccq_1_4),
      kccq_1_5 = ifelse(.data$kccq_1_5 == 9, NA_real_, .data$kccq_1_5),
      kccq_1_6 = ifelse(.data$kccq_1_6 == 9, NA_real_, .data$kccq_1_6),
      ## build scores
      ecu_kccq_phys_mean = round(mean(c(.data$kccq_1_1, .data$kccq_1_2, .data$kccq_1_3, .data$kccq_1_4, .data$kccq_1_5, .data$kccq_1_6), na.rm = TRUE), digits = 2),
      ecu_kccq_phys_score = ((.data$ecu_kccq_phys_mean - 1)/4)*100,
      # build score for symptom changes (question 2)
      ecu_kccq_sy_ch = round(ifelse(.data$kccq_2 == 6, 3, .data$kccq_2), digits = 2),
      ecu_kccq_sy_ch_score = round(((.data$ecu_kccq_sy_ch - 1)/4)*100, digits = 2),
      # build score for symptom frequency (questions 3, 5, 7 and 9)
      ## recode when no symptoms occured
      kccq_3 = ifelse(.data$kccq_3 == 5, NA_real_, .data$kccq_3),
      kccq_5 = ifelse(.data$kccq_5 == 7, NA_real_, .data$kccq_5),
      kccq_7 = ifelse(.data$kccq_7 == 7, NA_real_, .data$kccq_7),
      kccq_9 = ifelse(.data$kccq_9 == 5, NA_real_, .data$kccq_9),
      ## build scores
      ecu_kccq_3_score = round(((.data$kccq_3 - 1)/4)*100, digits = 2),
      ecu_kccq_5_score = round(((.data$kccq_5 - 1)/6)*100, digits = 2),
      ecu_kccq_7_score = round(((.data$kccq_7 - 1)/6)*100, digits = 2),
      ecu_kccq_9_score = round(((.data$kccq_9 - 1)/4)*100, digits = 2),
      ecu_kccq_sy_freq_score = round(mean(c(.data$ecu_kccq_3_score, .data$ecu_kccq_5_score, .data$ecu_kccq_7_score, .data$ecu_kccq_9_score), na.rm = TRUE), digits = 2),
      # build score for symptom severity (questions 4, 6 and 8)
      ## recode when there were no limitations
      kccq_4 = ifelse(.data$kccq_4 == 6, 5, .data$kccq_4),
      kccq_6 = ifelse(.data$kccq_6 == 6, 5, .data$kccq_6),
      kccq_8 = ifelse(.data$kccq_8 == 6, 5, .data$kccq_8),
      ## build score
      ecu_kccq_sy_sev_mean = round(mean(c(.data$kccq_4, .data$kccq_6, .data$kccq_8), na.rm = TRUE), digits = 2),
      ecu_kccq_sy_sev_score = round(((.data$ecu_kccq_sy_sev_mean - 1)/4)*100,digits = 2),
      # build score for symptom frequency and symptom severity
      ecu_kccq_sy_mean = round(mean(c(.data$ecu_kccq_sy_freq_score, .data$ecu_kccq_sy_sev_score), na.rm = TRUE), digits = 2),
      ecu_kccq_sy_score = round(.data$ecu_kccq_sy_mean / 2, digits = 2),
      # build score for self-efficacy (questions 10 and 11)
      ecu_kccq_se_mean = round(mean(c(.data$kccq_10, .data$kccq_11), na.rm = TRUE), digits = 2),
      ecu_kccq_se_score = round(((.data$ecu_kccq_se_mean - 1)/4)*100, digits = 2),
      # build score for quality of life (questions 12, 13, 14)
      ecu_kccq_qol_mean = round(mean(c(.data$kccq_12, .data$kccq_13, .data$kccq_14), na.rm = TRUE), digits = 2),
      ecu_kccq_qol_score = round(((.data$ecu_kccq_qol_mean - 1)/4)*100, digits = 2),
      # build score for social limitations (questions 15a to 15d)
      ## recode when question can not be answered
      kccq_15_1 = ifelse(.data$kccq_15_1 == 9, NA_real_, .data$kccq_15_1),
      kccq_15_2 = ifelse(.data$kccq_15_2 == 9, NA_real_, .data$kccq_15_2),
      kccq_15_3 = ifelse(.data$kccq_15_3 == 9, NA_real_, .data$kccq_15_3),
      kccq_15_4 = ifelse(.data$kccq_15_4 == 9, NA_real_, .data$kccq_15_4),
      ## build scores
      ecu_kccq_sl_mean = round(mean(c(.data$kccq_15_1, .data$kccq_15_2, .data$kccq_15_3, .data$kccq_15_4), na.rm = TRUE), digits = 2),
      ecu_kccq_sl_score = round(((.data$ecu_kccq_sl_mean - 1)/4)*100, digits = 2),
      # build total score
      ecu_kccq_total = round(mean(c(.data$ecu_kccq_phys_score, .data$ecu_kccq_sy_freq_score, .data$ecu_kccq_sy_sev_score, .data$ecu_kccq_qol_score, .data$ecu_kccq_sl_score), na.rm = TRUE), digits = 2)) %>%
    ungroup()
  
  return(trial_data)
  
}


#' Primary coding Six Item Loneliness Scale (6 ILS)
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_six_ils1,ecu_six_ils2, ecu_six_ils3, ecu_six_ils4, ecu_six_ils5, ecu_six_ils6, ecu_six_ils_total, ecu_six_ils_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_6ils <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_six_ils1 = recode_6ils(.data$six_ils1.factor, version = "neg"),
           ecu_six_ils2 = recode_6ils(.data$six_ils2.factor, version = "pos"),
           ecu_six_ils3 = recode_6ils(.data$six_ils3.factor, version = "pos"),
           ecu_six_ils4 = recode_6ils(.data$six_ils4.factor, version = "neg"),
           ecu_six_ils5 = recode_6ils(.data$six_ils5.factor, version = "pos"),
           ecu_six_ils6 = recode_6ils(.data$six_ils6.factor, version = "neg"),
           ecu_six_ils_total = calculate_6ils_total(.data$ecu_six_ils1, .data$ecu_six_ils2, .data$ecu_six_ils3, .data$ecu_six_ils4, .data$ecu_six_ils5, .data$ecu_six_ils6),
           ecu_six_ils_cat = categorize_6ils_ecu(.data$ecu_six_ils_total)) 
  
  return(trial_data)
}


#' Primary coding Perceived Stress Scale (PSS)
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_pss1,ecu_pss2, ecu_pss3, ecu_pss4, ecu_pss5, ecu_pss6, ecu_pss_total, ecu_pss_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_pss <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(ecu_pss1 = recode_pss(.data$pss01.factor, version = 1),
           ecu_pss2 = recode_pss(.data$pss02.factor, version = 1),
           ecu_pss3 = recode_pss(.data$pss03.factor, version = 1),
           ecu_pss4 = recode_pss(.data$pss04.factor, version = 2),
           ecu_pss5 = recode_pss(.data$pss05.factor, version = 2),
           ecu_pss6 = recode_pss(.data$pss06.factor, version = 1),
           ecu_pss7 = recode_pss(.data$pss07.factor, version = 2),
           ecu_pss8 = recode_pss(.data$pss08.factor, version = 2),
           ecu_pss9 = recode_pss(.data$pss09.factor, version = 1),
           ecu_pss10 = recode_pss(.data$pss10.factor, version = 1),
           ecu_pss_total = calculate_pss_total(.data$ecu_pss1,.data$ecu_pss2, .data$ecu_pss3, .data$ecu_pss4, .data$ecu_pss5, .data$ecu_pss6, 
                                               .data$ecu_pss7, .data$ecu_pss8, .data$ecu_pss9, .data$ecu_pss10),
           ecu_pss_cat = categorize_pss_ecu(.data$ecu_pss_total)) 
  
  return(trial_data)
  
}


#' Primary coding Global Physical Activity Questionnaire (GPAQ) pre COVID
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_gpaq_p3, ecu_gpaq_p6, ecu_gpaq_p9, ecu_gpaq_p12, ecu_gpaq_p15, ecu_gpaq_p16, ecu_gpaq_p2cln, ecu_gpaq_p3cln
#' ecu_gpaq_p5cln, ecu_gpaq_p6cln, ecu_gpaq_p8cln, ecu_gpaq_p9cln, ecu_gpaq_p11cln, ecu_gpaq_p12cln, ecu_gpaq_p14cln
#' ecu_gpaq_p15cln, ecu_gpaq_valid, ecu_gpaq_p16cln, ecu_gpaq_p1t3cln, ecu_gpaq_p4t6cln, ecu_gpaq_p7t9cln, ecu_gpaq_p10t12cln
#' ecu_gpaq_p13t15cln, ecu_gpaq_p1t3_met, ecu_gpaq_p4t6_met, ecu_gpaq_p7t9_met, ecu_gpaq_p10t12_met, ecu_gpaq_p13t15_met
#' ecu_gpaq_ptotal_met, ecu_gpaq_ptotalday_met, ecu_gpaq_pworkday_met, ecu_gpaq_ptravelday_met, ecu_gpaq_precday_met
#' ecu_gpaq_per_work_met, ecu_gpaq_per_trans_met, ecu_gpaq_per_rec_met, ecu_gpaq_met_who, ecu_gpaq_p1t3_uw, ecu_gpaq_p4t6_uw
#' ecu_gpaq_p7t9_uw, ecu_gpaq_p10t12_uw, ecu_gpaq_p13t15_uw, ecu_gpaq_ptotal_uw, ecu_gpaq_ptotalday_uw, ecu_gpaq_pworkday_uw
#' ecu_gpaq_ptravelday_uw, ecu_gpaq_precday_uw, ecu_gpaq_per_work_uw, ecu_gpaq_per_trans_uw, ecu_gpaq_per_rec_uw
#' ecu_gpaq_work, ecu_gpaq_trans, ecu_gpaq_rec, ecu_gpaq_vig_activ, ecu_gpaq_cln, ecu_gpaq_cln_sedentary
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_gpaq_pre <- function(trial_data) {
  
  trial_data <- primary_coding_pop_gpaq_vars_pre(trial_data)
  trial_data <- primary_coding_pop_gpaq_calc(trial_data)
  
  return(trial_data)
  
}


#' Primary coding Global Physical Activity Questionnaire (GPAQ) post COVID
#' 
#' adds the following column to surveyfrageboge: 
#' ecu_gpaq_p3_post, ecu_gpaq_p6_post, ecu_gpaq_p9_post, ecu_gpaq_p12_post, ecu_gpaq_p15_post, ecu_gpaq_p16_post, ecu_gpaq_p2cln_post
#' ecu_gpaq_p3cln_post, ecu_gpaq_p5cln_post, ecu_gpaq_p6cln_post, ecu_gpaq_p8cln_post, ecu_gpaq_p9cln_post, ecu_gpaq_p11cln_post
#' ecu_gpaq_p12cln_post, ecu_gpaq_p14cln_post, ecu_gpaq_p15cln_post, ecu_gpaq_valid_post, ecu_gpaq_p16cln_post, ecu_gpaq_p1t3cln_post 
#' ecu_gpaq_p4t6cln_post, ecu_gpaq_p7t9cln_post, ecu_gpaq_p10t12cln_post, ecu_gpaq_p13t15cln_post, ecu_gpaq_p1t3_met_post, ecu_gpaq_p4t6_met_post
#' ecu_gpaq_p7t9_met_post, ecu_gpaq_p10t12_met_post, ecu_gpaq_p13t15_me_post, ecu_gpaq_ptotal_met_post, ecu_gpaq_ptotalday_met_post
#' ecu_gpaq_pworkday_met_post, ecu_gpaq_ptravelday_met_post, ecu_gpaq_precday_met_post, ecu_gpaq_per_work_met_post, ecu_gpaq_per_trans_met_post
#' ecu_gpaq_per_rec_met_post, ecu_gpaq_met_who_post, ecu_gpaq_p1t3_uw_post, ecu_gpaq_p4t6_uw_post, ecu_gpaq_p7t9_uw_post, ecu_gpaq_p10t12_uw_post
#' ecu_gpaq_p13t15_uw_post, ecu_gpaq_ptotal_uw_post, ecu_gpaq_ptotalday_uw_post, ecu_gpaq_pworkday_uw_post, ecu_gpaq_ptravelday_uw_post
#' ecu_gpaq_precday_uw_post, ecu_gpaq_per_work_uw_post, ecu_gpaq_per_trans_uw_post, ecu_gpaq_per_rec_uw_post, ecu_gpaq_work_post, ecu_gpaq_trans_post 
#' ecu_gpaq_rec_post, ecu_gpaq_vig_activ_post, ecu_gpaq_cln_post, ecu_gpaq_cln_sedentary_post
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_pop_gpaq_post <- function(trial_data) {
  
  trial_data <- primary_coding_pop_gpaq_vars_post(trial_data)
  trial_data <- primary_coding_pop_gpaq_calc(trial_data)
  
  trial_data$surveyfrageboge <- trial_data$surveyfrageboge %>%
    rename_with(~ paste0(., "_post"), .data$ecu_gpaq_p3:.data$ecu_gpaq_cln_sedentary)
  
  return(trial_data)
  
}


#' 6 Minute Walk Test (6MWT)
#' 
#' adds the following columns to geria:
#' ecu_6mwt_soll, ecu_6mwt_soll_erreicht
#' 
#' @param trial_data a SsecuTrial data object
#' @param pid column name of patient ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_pop_6mwt <- function(trial_data, pid) {
  
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$erstbefragung), "mnpvislabel", "visit_name")
  
  needed_vars <- trial_data$geria %>%
    mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")) %>%
    select(pid, .data$visit_name_temp) %>% 
    left_join(trial_data$erstbefragung %>% select(pid, "gec_demo_age", visit_label_var_name) %>% mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")), by = c(pid, "visit_name_temp")) %>%
    left_join(trial_data$anthropo %>% select(pid, "gec_height", visit_label_var_name)%>% mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")), by = c(pid, "visit_name_temp")) %>%
    select(pid, .data$visit_name_temp, .data$gec_demo_age, .data$gec_height)
  
  trial_data$geria <- trial_data$geria %>%
    mutate(visit_name_temp = str_remove(!!sym(visit_label_var_name), "-EB|-VO")) %>%
    left_join(needed_vars, by = c(pid, "visit_name_temp")) %>%
    mutate(ecu_6mwt_soll = 592.134 + (0.203 * (.data$gec_demo_age < 56.2) * (56.2 - .data$gec_demo_age)) -
             (5.034 * (.data$gec_demo_age > 56.2) * (.data$gec_demo_age - 56.2)) + 1.857 * (.data$gec_height - 172.6),
           ecu_6mwt_soll_erreicht = case_when(.data$bew_6mwt_gesamtstr >= .data$ecu_6mwt_soll ~ 1,
                                              .data$bew_6mwt_gesamtstr < .data$ecu_6mwt_soll ~ 0)) %>%
    select(-.data$visit_name_temp)
  
  return(trial_data)
  
}


# POP Wrapper primary coding ==================================================

#' Primary coding POP Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#' 
#' 
#'
#' @param trial_data The secu trial data object
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @importFrom forcats fct_reorder
#' @export

primary_coding_pop <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$erstbefragung), "mnpvislabel", "visit_name")
  
  ## Demographics ==============================================================
  ### Age ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_age(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_age() did not work. This is likely due to missing variables.")
             print(e)})
  ### Migration ================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_migration(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_migration() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## BMI =======================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_bmi(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Abdominal overweight ======================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_abd_overw(trial_data, pid)},
           error = function(e) {
             warning("primary_coding_pop_abd_overw() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Clinical parameters =======================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_clinical_params(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_clinical_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Cardiological parameters ==================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_cardio_params(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_cardio_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Pneumoogical parameters ===================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_pneumo_params(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_pneumo_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Scores ====================================================================
  ### EQ-5D-5L =================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_eq5d5l(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_eq5d5l() did not work. This is likely due to missing variables.")
             print(e)})
  ### MoCA =====================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_moca(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_moca() did not work. This is likely due to missing variables.")
             print(e)})
  ### MMRC =====================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_mmrc(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_mmrc() did not work. This is likely due to missing variables.")
             print(e)})
  ### PHQ-8 ====================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_phq8(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_phq8() did not work. This is likely due to missing variables.")
             print(e)})
  ### GAD-7 ====================================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_gad7(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_gad7() did not work. This is likely due to missing variables.")
             print(e)})
  ### FACIT-Fatigue Scale ======================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_facitf(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_facitf() did not work. This is likely due to missing variables.")
             print(e)})
  ### Brief Resilience Scale ===================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_brs(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_brs() did not work. This is likely due to missing variables.")
             print(e)})
  ### Pittsburg Sleep Quality Index ============================================
  tryCatch(expr = {trial_data <- primary_coding_pop_psqi(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_psqi() did not work. This is likely due to missing variables.")
             print(e)})
  ### Six-Item Loneliness Scale ================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_6ils(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_6ils() did not work. This is likely due to missing variables.")
             print(e)})
  ### Perceived Stress Scale ===================================================
  tryCatch(expr = {trial_data <- primary_coding_pop_pss(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_pss() did not work. This is likely due to missing variables.")
             print(e)})
  ### Global Physical Activity Questionnaire ===================================
  tryCatch(expr = {trial_data <- primary_coding_pop_gpaq_post(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_gpaq_post () did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_pop_gpaq_pre(trial_data)},
           error = function(e) {
             warning("primary_coding_pop_gpaq_pre () did not work. This is likely due to missing variables.")
             print(e)})
  
  
  catw("Primary Coding done")
  
  return(trial_data)
}


# POP helper functions for primary coding ==================================================

#' Primary codes migration background from nationality and country of birth
#'
#' @param staatsang vector "Ist Ihre Staatsangehörigkeit Deutsch?" w/ levels Ja (1); Nein, eine andere (0)
#' @param gebland vector "Sind Sie in Deutschland geboren?" w/ levels Ja (1); Nein, in einem anderen Land (0)
#' @param gebland2 vector "Ist mind. ein Elternteil von Ihnen außerhalb Deutschlands geboren?" w/ levels Ja (1); Nein (0); Weiß nicht (-1)
#' @return A factorized vector w/ levels "Deutschland", 19 different nationalities, or "other"

get_ecu_background_pop <- function(staatsang, gebland, gebland2){
  ecu_migration_background <- factor(case_when(staatsang == 0 | gebland == 0 | gebland2 == 1 ~ "Ja", 
                                               TRUE ~ "Nein"))
  return(ecu_migration_background)
}


#' Primary coding GPAQ variable/column selection for GPAQ pre COVID
#' 
#' @param trial_data A secuTrial data object

primary_coding_pop_gpaq_vars_pre <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(p1 = .data$gpaq1,
           p2 = .data$gpaq2a,
           p3a = .data$gpaq3a_hh,
           p3b = .data$gpaq3a_mm,
           p4 = .data$gpaq4,
           p5 = .data$gpaq5a,
           p6a = .data$gpaq6a_hh,
           p6b = .data$gpaq6a_mm,
           p7 = .data$gpaq7,
           p8 = .data$gpaq8a,
           p9a = .data$gpaq9a_hh,
           p9b = .data$gpaq9a_mm,
           p10 = .data$gpaq10,
           p11 = .data$gpaq11a,
           p12a = .data$gpaq12a_hh,
           p12b = .data$gpaq12a_mm,
           p13 = .data$gpaq13,
           p14 = .data$gpaq14a,
           p15a = .data$gpaq15a_hh,
           p15b = .data$gpaq15a_mm,
           p16a = .data$gpaq16a_hh,
           p16b = .data$gpaq16a_mm) 
  
  return(trial_data)
}


#' Primary coding GPAQ variable/column selection for GPAQ post COVID
#' 
#' @param trial_data A secuTrial data object

primary_coding_pop_gpaq_vars_post <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    mutate(p1 = .data$gpaq1,
           p2 = .data$gpaq2b,
           p3a = .data$gpaq3b_hh,
           p3b = .data$gpaq3b_mm,
           p4 = .data$gpaq4,
           p5 = .data$gpaq5b,
           p6a = .data$gpaq6b_hh,
           p6b = .data$gpaq6b_mm,
           p7 = .data$gpaq7,
           p8 = .data$gpaq8b,
           p9a = .data$gpaq9b_hh,
           p9b = .data$gpaq9b_mm,
           p10 = .data$gpaq10,
           p11 = .data$gpaq11b,
           p12a = .data$gpaq12b_hh,
           p12b = .data$gpaq12b_mm,
           p13 = .data$gpaq13,
           p14 = .data$gpaq14b,
           p15a = .data$gpaq15b_hh,
           p15b = .data$gpaq15b_mm,
           p16a = .data$gpaq16b_hh,
           p16b = .data$gpaq16b_mm)
  
  return(trial_data)
}


#' Primary coding GPAQ calculation
#' 
#' @param trial_data A secuTrial data object

primary_coding_pop_gpaq_calc <- function(trial_data) {
  
  trial_data[["surveyfrageboge"]] <- trial_data[["surveyfrageboge"]] %>%
    
    mutate (
      # create minute variables for vigorous work activity
      p3amin = case_when(is.na(.data$p3a) ~ 0, TRUE ~ .data$p3a*60),
      p3bmin = case_when(is.na(.data$p3b) ~ 0, TRUE ~ .data$p3b),
      ecu_gpaq_p3 = .data$p3amin + .data$p3bmin,
      # create minute variables for moderate work activity
      p6amin = case_when(is.na(.data$p6a) ~ 0, TRUE ~ .data$p6a*60),
      p6bmin = case_when(is.na(.data$p6b) ~ 0, TRUE ~ .data$p6b), 
      ecu_gpaq_p6 = .data$p6amin + .data$p6bmin, 
      # create minute variables for travel activity
      p9amin = case_when(is.na(.data$p9a) ~ 0, TRUE ~ .data$p9a*60),
      p9bmin = case_when(is.na(.data$p9b) ~ 0, TRUE ~ .data$p9b),
      ecu_gpaq_p9 = .data$p9amin + .data$p9bmin, 
      # create minute variables for vigorous recreation activity
      p12amin = case_when(is.na(.data$p12a) ~ 0, TRUE ~ .data$p12a*60),
      p12bmin = case_when(is.na(.data$p12b) ~ 0, TRUE ~ .data$p12b),
      ecu_gpaq_p12 = .data$p12amin + .data$p12bmin, 
      # create minute variable for moderate recreation activity
      p15amin = case_when(is.na(.data$p15a) ~ 0, TRUE ~ .data$p15a*60),
      p15bmin = case_when(is.na(.data$p15b) ~ 0, TRUE ~ .data$p15b),
      ecu_gpaq_p15 = .data$p15amin + .data$p15bmin, 
      # create minute variable for sedentary activity
      p16amin = case_when(is.na(.data$p16a) ~ 0, TRUE ~ .data$p16a*60),
      p16bmin = case_when(is.na(.data$p16b) ~ 0, TRUE ~ .data$p16b),
      ecu_gpaq_p16 = .data$p16amin + .data$p16bmin,
      
      # check for valid response to p2
      ecu_gpaq_p2cln = case_when((.data$p1 == 1 & (.data$p2 > 0 & .data$p2 < 8)) | (.data$p1 == 0 & (.data$p2 == 0 | is.na(.data$p2))) ~ 1, TRUE ~ 2),
      # check for valid response to p3
      ecu_gpaq_p3cln = case_when((.data$ecu_gpaq_p2cln == 1 & .data$p2 > 0 & .data$p2 < 8 & .data$ecu_gpaq_p3 > 9 & .data$ecu_gpaq_p3 < 961) | 
                                   (.data$ecu_gpaq_p2cln == 1 & (.data$p2 == 0 | is.na(.data$p2)) & .data$ecu_gpaq_p3 == 0) ~ 1, TRUE ~ 2),
      # check for valid response to p5
      ecu_gpaq_p5cln = case_when((.data$p4 == 1 & (.data$p5 > 0 & .data$p5 < 8)) | (.data$p4 == 0 & (.data$p5 == 0 | is.na(.data$p5))) ~ 1, TRUE ~ 2),
      # check for valid response to p6
      ecu_gpaq_p6cln = case_when((.data$ecu_gpaq_p5cln == 1 & .data$p5 > 0 & .data$p5 < 8 & .data$ecu_gpaq_p6 > 9 & .data$ecu_gpaq_p6 < 961) | 
                                   (.data$ecu_gpaq_p5cln == 1 & (.data$p5 == 0 | is.na(.data$p5)) & .data$ecu_gpaq_p6 == 0) ~ 1, TRUE ~ 2),
      # check for valid response to p8
      ecu_gpaq_p8cln = case_when((.data$p7 == 1 & (.data$p8 > 0 & .data$p8 < 8)) | (.data$p7 == 0 & (.data$p8 == 0 | is.na(.data$p8))) ~ 1, TRUE ~ 2),
      # check for valid response to p9
      ecu_gpaq_p9cln = case_when((.data$ecu_gpaq_p8cln == 1 & .data$p8 > 0 & .data$p8 < 8 & .data$ecu_gpaq_p9 > 9 & .data$ecu_gpaq_p9 < 961) | 
                                   (.data$ecu_gpaq_p8cln == 1 & (.data$p8 == 0 | is.na(.data$p8)) & .data$ecu_gpaq_p9 == 0) ~ 1, TRUE ~ 2),
      # check for valid response to p11
      ecu_gpaq_p11cln = case_when((.data$p10 == 1 & (.data$p11 > 0 & .data$p11 < 8)) | (.data$p10 == 0 & (.data$p11 == 0 | is.na(.data$p11))) ~ 1, TRUE ~ 2),
      # check for valid response to p12
      ecu_gpaq_p12cln = case_when((.data$ecu_gpaq_p11cln == 1 & .data$p11 > 0 & .data$p11 < 8 & .data$ecu_gpaq_p12 > 9 & .data$ecu_gpaq_p12 < 961) | 
                                    (.data$ecu_gpaq_p11cln == 1 & (.data$p11 == 0 | is.na(.data$p11)) & .data$ecu_gpaq_p12 == 0) ~ 1, TRUE ~ 2),
      # check for valid response to p14
      ecu_gpaq_p14cln = case_when((.data$p13 == 1 & (.data$p14 > 0 & .data$p14 < 8)) | (.data$p13 == 0 & (.data$p14 == 0 | is.na(.data$p14))) ~ 1, TRUE ~ 2),
      # check for valid response to p15
      ecu_gpaq_p15cln = case_when((.data$ecu_gpaq_p14cln == 1 & .data$p14 > 0 & .data$p14 < 8 & .data$ecu_gpaq_p15 > 9 & .data$ecu_gpaq_p15 < 961) | 
                                    (.data$ecu_gpaq_p14cln == 1 & (.data$p14 == 0 | is.na(.data$p14)) & .data$ecu_gpaq_p15 == 0) ~ 1, TRUE ~ 2),
      
      # check whether at least one sub-domain has a valid answer (either no activity ("0") or activity ("1") with information about days and minutes per week)
      ecu_gpaq_valid = case_when((.data$ecu_gpaq_p2cln == 1 & .data$ecu_gpaq_p3cln == 1) | # vigorous work activity with days and minutes per week or no vigorous work activity AND less than 960 minutes per week 
                                   (.data$ecu_gpaq_p5cln == 1 & .data$ecu_gpaq_p6cln == 1) | # moderate work activity days and with minutes per week or no moderate work activity AND less than 960 minutes per week 
                                   (.data$ecu_gpaq_p8cln == 1 & .data$ecu_gpaq_p9cln == 1) | # travel activity with days and minutes per week or no travel activity AND less than 960 minutes per week 
                                   (.data$ecu_gpaq_p11cln == 1 & .data$ecu_gpaq_p12cln == 1) | # vigorous recreation activity with days and minutes per week or no vigorous recreation activity AND less than 960 minutes per week 
                                   (.data$ecu_gpaq_p14cln == 1 & .data$ecu_gpaq_p15cln == 1) ~ 1, # moderate recreation activity with days and minutes per week oder no moderate recreation activity AND less than 960 minutes per week 
                                 TRUE ~ 2),
      # check for valid response to p1 through p3a & p3b
      ecu_gpaq_p1t3cln = case_when((.data$ecu_gpaq_p3cln == 1 & .data$ecu_gpaq_valid == 1) | (is.na(.data$p1) & (.data$p2 == 0 | is.na(.data$p2)) & .data$ecu_gpaq_p3 == 0 & .data$ecu_gpaq_valid == 1) ~ 1, TRUE ~ 2),
      # check for valid response to p4 through p6a & p6b
      ecu_gpaq_p4t6cln = case_when((.data$ecu_gpaq_p5cln == 1 & .data$ecu_gpaq_valid == 1) | (is.na(.data$p4) & (.data$p5 == 0 | is.na(.data$p5)) & .data$ecu_gpaq_p6 == 0 & .data$ecu_gpaq_valid == 1) ~ 1, TRUE ~ 2),
      # check for valid response to p7 through p9a & p9b
      ecu_gpaq_p7t9cln = case_when((.data$ecu_gpaq_p8cln == 1 & .data$ecu_gpaq_valid == 1) | (is.na(.data$p7) & (.data$p8 == 0 | is.na(.data$p8)) & .data$ecu_gpaq_p9 == 0 & .data$ecu_gpaq_valid == 1) ~ 1, TRUE ~ 2),
      # check for valid response to p10 through p12a & p12b
      ecu_gpaq_p10t12cln = case_when((.data$ecu_gpaq_p11cln == 1 & .data$ecu_gpaq_valid == 1) | (is.na(.data$p10) & (.data$p11 == 0 | is.na(.data$p11)) & .data$ecu_gpaq_p12 == 0 & .data$ecu_gpaq_valid == 1) ~ 1, TRUE ~ 2),
      # check for valid response to p13 through p15a & p15b
      ecu_gpaq_p13t15cln = case_when((.data$ecu_gpaq_p14cln == 1 & .data$ecu_gpaq_valid == 1) | (is.na(.data$p13) & (.data$p14 == 0 | is.na(.data$p14)) & .data$ecu_gpaq_p15 == 0 & .data$ecu_gpaq_valid == 1) ~ 1, TRUE ~ 2),
      # check for valid response to p16
      ecu_gpaq_p16cln = case_when(.data$ecu_gpaq_p16 < 1441 & .data$ecu_gpaq_valid == 1 ~ 1, TRUE ~ 2),
      
      # built MET-value variables for activity per week 
      ## MET value for vigorous work activity per week
      ecu_gpaq_p1t3_met = case_when(.data$ecu_gpaq_p1t3cln == 1 ~ .data$p2 * .data$ecu_gpaq_p3 * 8, TRUE ~ NA_real_),
      ## MET value for moderate work activity per week
      ecu_gpaq_p4t6_met = case_when(.data$ecu_gpaq_p4t6cln == 1 ~ .data$p5 * .data$ecu_gpaq_p6 * 4, TRUE ~ NA_real_),
      ## MET value for travel activity per week
      ecu_gpaq_p7t9_met = case_when(.data$ecu_gpaq_p7t9cln == 1 ~ .data$p8 * .data$ecu_gpaq_p9 * 4, TRUE ~ NA_real_),
      ## MET value for vigorous recreation activity per week 
      ecu_gpaq_p10t12_met = case_when(.data$ecu_gpaq_p10t12cln == 1 ~ .data$p11 * .data$ecu_gpaq_p12 * 8, TRUE ~ NA_real_),
      ## MET value for moderate recreation activity per week
      ecu_gpaq_p13t15_met = case_when(.data$ecu_gpaq_p13t15cln == 1 ~ .data$p14 * .data$ecu_gpaq_p15 * 4, TRUE ~ NA_real_),
      ## MET value for total activity per week 
      ecu_gpaq_ptotal_met = ifelse(is.na(.data$ecu_gpaq_p1t3_met), 0, .data$ecu_gpaq_p1t3_met) +
        ifelse(is.na(.data$ecu_gpaq_p4t6_met), 0, .data$ecu_gpaq_p4t6_met) +
        ifelse(is.na(.data$ecu_gpaq_p7t9_met), 0, .data$ecu_gpaq_p7t9_met) +
        ifelse(is.na(.data$ecu_gpaq_p10t12_met), 0, .data$ecu_gpaq_p10t12_met) +
        ifelse(is.na(.data$ecu_gpaq_p13t15_met), 0, .data$ecu_gpaq_p13t15_met),
      ecu_gpaq_ptotal_met = case_when(.data$ecu_gpaq_valid == 0 ~ NA, TRUE ~ .data$ecu_gpaq_ptotal_met),
      ## MET value for average activity per day
      ecu_gpaq_ptotalday_met = .data$ecu_gpaq_ptotal_met / 7,
      ## MET value for average work-related activity per day
      ecu_gpaq_pworkday_met = (.data$ecu_gpaq_p1t3_met + .data$ecu_gpaq_p4t6_met) / 7,
      ## MET value for average transport-related activity per day
      ecu_gpaq_ptravelday_met = .data$ecu_gpaq_p7t9_met / 7,
      ## MET value for average recreation-related activity per day
      ecu_gpaq_precday_met = (.data$ecu_gpaq_p10t12_met + .data$ecu_gpaq_p13t15_met) / 7,
      ## MET value for percent of all activity from work-related activities
      ecu_gpaq_per_work_met = round(((.data$ecu_gpaq_p1t3_met + .data$ecu_gpaq_p4t6_met) / .data$ecu_gpaq_ptotal_met) * 100, digits = 2),
      ## MET value for percent of all activity from transportation-related activities
      ecu_gpaq_per_trans_met = round((.data$ecu_gpaq_p7t9_met / .data$ecu_gpaq_ptotal_met) * 100, digits = 2),
      ## MET value for percent of all activity from recreation-related activities
      ecu_gpaq_per_rec_met = round(((.data$ecu_gpaq_p10t12_met + .data$ecu_gpaq_p13t15_met) / .data$ecu_gpaq_ptotal_met) * 100, digits = 2),
      
      # check if total MET activity meets who recommendations 
      ecu_gpaq_met_who = case_when(.data$ecu_gpaq_ptotal_met < 600 ~ "Does not meet recommendations",
                                   .data$ecu_gpaq_ptotal_met >= 600 ~ "Meets recommendations"),
      
      # built unweighted value variables for activity per week
      ## unweighted value for vigorous work activity per week 
      ecu_gpaq_p1t3_uw = case_when(.data$ecu_gpaq_p1t3cln == 1 ~ .data$p2 * .data$ecu_gpaq_p3, TRUE ~ NA),
      ## unweighted value for moderate work activity per week
      ecu_gpaq_p4t6_uw = case_when(.data$ecu_gpaq_p4t6cln == 1 ~ .data$p5 * .data$ecu_gpaq_p6, TRUE ~ NA),
      ## unweighted value for travel activity per week
      ecu_gpaq_p7t9_uw = case_when(.data$ecu_gpaq_p7t9cln == 1 ~ .data$p8 * .data$ecu_gpaq_p9, TRUE ~ NA),
      ## unweighted value for vigorous recreation activity per week 
      ecu_gpaq_p10t12_uw = case_when(.data$ecu_gpaq_p10t12cln == 1 ~ .data$p11 * .data$ecu_gpaq_p12, TRUE ~ NA),
      ## unweighted value for moderate recreation activity per week
      ecu_gpaq_p13t15_uw = case_when(.data$ecu_gpaq_p13t15cln == 1 ~ .data$p14 * .data$ecu_gpaq_p15, TRUE ~ NA),
      ## unweighted value for total activity per week
      ecu_gpaq_ptotal_uw = ifelse(is.na(.data$ecu_gpaq_p1t3_uw), 0, .data$ecu_gpaq_p1t3_uw) +
        ifelse(is.na(.data$ecu_gpaq_p4t6_uw), 0, .data$ecu_gpaq_p4t6_uw) +
        ifelse(is.na(.data$ecu_gpaq_p7t9_uw), 0, .data$ecu_gpaq_p7t9_uw) +
        ifelse(is.na(.data$ecu_gpaq_p10t12_uw), 0, .data$ecu_gpaq_p10t12_uw) +
        ifelse(is.na(.data$ecu_gpaq_p13t15_uw), 0, .data$ecu_gpaq_p13t15_uw),
      ecu_gpaq_ptotal_uw = case_when(.data$ecu_gpaq_valid == 0 ~ NA, TRUE ~ .data$ecu_gpaq_ptotal_uw), 
      ## unweighted value for average activity per day
      ecu_gpaq_ptotalday_uw = .data$ecu_gpaq_ptotal_uw / 7,
      ## unweighted value for average work-related activity per day
      ecu_gpaq_pworkday_uw = (.data$ecu_gpaq_p1t3_uw + .data$ecu_gpaq_p4t6_uw) / 7,
      ## unweighted value for average transport-related activity per day
      ecu_gpaq_ptravelday_uw = .data$ecu_gpaq_p7t9_uw / 7,
      ## unweighted value for average recreation-related activity per day
      ecu_gpaq_precday_uw = (.data$ecu_gpaq_p10t12_uw + .data$ecu_gpaq_p13t15_uw) / 7,
      ## unweighted value for percent of all activity from work-related activities
      ecu_gpaq_per_work_uw = round(((.data$ecu_gpaq_p1t3_uw + .data$ecu_gpaq_p4t6_uw) / .data$ecu_gpaq_ptotal_uw) * 100, digits = 2),
      ## unweighted value for percent of all activity from transportation-related activities
      ecu_gpaq_per_trans_uw = round((.data$ecu_gpaq_p7t9_uw / .data$ecu_gpaq_ptotal_uw) * 100, digits = 2), 
      ## unweighted value for percent of all activity from recreation-related activities
      ecu_gpaq_per_rec_uw = round(((.data$ecu_gpaq_p10t12_uw + .data$ecu_gpaq_p13t15_uw) / .data$ecu_gpaq_ptotal_uw) * 100, digits = 2),
      
      # check if respondent did any activity per subdomain
      # TODO: wenn valid == 0 (nicht valid), dann NA (sonst bekommen auch diese ein Label)
      ## indicator for any work-related activity
      ecu_gpaq_work = case_when(.data$p1 == 1 | .data$p4 == 1 ~ "Did work activity", .data$ecu_gpaq_valid == 0 ~ NA, TRUE ~ "Did no work activity"),
      ## indicator for any transportation-related activity
      ecu_gpaq_trans = case_when(.data$p7 == 1 ~ "Did transport activity", .data$ecu_gpaq_valid == 0 ~ NA, TRUE ~ "Did no transport activity"),
      ## indicator for any recreation-related activity
      ecu_gpaq_rec = case_when(.data$p10 == 1 | .data$p13 == 1 ~ "Did recreation activity", .data$ecu_gpaq_valid == 0 ~ NA, TRUE ~ "Did no recreation activity"),
      
      # check if respondents did any vigorous physical activity
      ecu_gpaq_vig_activ = case_when(.data$p1 == 1 | .data$p10 == 1 ~ "Did vigorous physical activity", .data$ecu_gpaq_valid == 0 ~ NA, TRUE ~ "Did no vigorous physical activity"),
      
      # check to see if all physical activitiy responses, as a combined set, are valid 
      # (all subsets of respones must be clean and at least one subset of responses must have a resonse (not missing))
      ecu_gpaq_cln = case_when(.data$ecu_gpaq_valid == 1 & .data$ecu_gpaq_p1t3cln == 1 & .data$ecu_gpaq_p4t6cln == 1 & .data$ecu_gpaq_p7t9cln == 1 & .data$ecu_gpaq_p10t12cln == 1 & .data$ecu_gpaq_p13t15cln == 1 & 
                                 (!is.na(.data$p1) | !is.na(.data$p4) | !is.na(.data$p7) | !is.na(.data$p10) | !is.na(.data$p13)) ~ 1, 
                               TRUE ~ 2),
      ecu_gpaq_cln_sedentary = case_when(.data$ecu_gpaq_valid == 1 & .data$ecu_gpaq_p16cln == 1 ~ 1, TRUE ~ 2)
    ) %>%
    select(-(.data$p1:.data$p16b), -c(.data$p3amin, .data$p3bmin, .data$p6amin, .data$p6bmin, .data$p9amin, .data$p9bmin, .data$p12amin, .data$p12bmin, .data$p15amin, .data$p15bmin, .data$p16amin, .data$p16bmin))
  
  return(trial_data)
} 
