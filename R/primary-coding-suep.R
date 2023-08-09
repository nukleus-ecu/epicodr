#################
# Primary coding for NAPKON SUEP secuTrial data via secuTrialR =================
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

# structure of functions with the prefix primary_coding_suep 
# The functions create one or more new columns (using mutate), which are temporarily saved under "new_vars_to_add" together with the visitid 
# result is then merged to the "form_to_add_vars", a data.frame within trial_data
# the functions return the trial data including new variables
# in the last step of this script, in the function primary_coding_suep(), all selected primary_coding_suep steps are performed consecutively. 
# this is the function which will be called in the run script to create trial_data from trial_data_raw

# Demographics =================================================================

## Age =========================================================================

#' Primary coding age
#'
#' adds the following columns to bv1: ecu_age - age in years, ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_age <- function(trial_data, pid, visitid) {
  
  formname_to_add_vars <- "bv1"
  formname_with_needed_vars <- "scv"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  form_with_needed_vars <- trial_data[[formname_with_needed_vars]] 
  
  new_vars_to_add <- form_to_add_vars %>%
    left_join(select(form_with_needed_vars, -matches(visitid)), by=pid) %>% 
    mutate(
      ecu_age = calculate_full_years(from = .data$gec_birthdate.date, to = .data$pr_incl_date.date),
      ecu_age_cat_dec = ecu_age_cat_dec(.data$ecu_age),
      ecu_age_cat_3 = ecu_age_cat_3(.data$ecu_age)
    ) %>%
    select(matches(visitid), .data$ecu_age, .data$ecu_age_cat_dec, .data$ecu_age_cat_3)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


## BMI =========================================================================

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to fuv1: ecu_bmi, ecu_bmi_est, ecu_bmi_cat, ecu_bmi_est_cat
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_bmi <- function(trial_data, pid, visitid) {
  
  formname_to_add_vars <- "fuv1"
  formname_with_needed_vars <- "bv1"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  form_with_needed_vars <- trial_data[[formname_with_needed_vars]] 
  
  new_vars_to_add <- form_to_add_vars %>%
    left_join(select(form_with_needed_vars, -matches(visitid)), by=pid) %>% # bv1 contains only one entry for height per pat. But height is needed for every entry in fuv1. Therefore, we have to join by pat id and drop the visit id from bv1 to avoid var name issues when joingn back after calculation.
    mutate(ecu_bmi = calculate_bmi(.data$gec_weight, .data$gec_height),
           ecu_bmi_est = calculate_bmi(.data$weight_estimated, .data$gec_height),
           ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi, .data$gec_height_uk.factor, .data$gec_weight_uk.factor),
           ecu_bmi_est_cat = categorize_bmi_ecu(.data$ecu_bmi_est, .data$gec_height_uk.factor, .data$weight_estimated_uk.factor )
    )%>% 
    select(matches(visitid), .data$ecu_bmi, .data$ecu_bmi_est, .data$ecu_bmi_cat, .data$ecu_bmi_est_cat)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


# Clinical Parameters ==========================================================

# the following clinical parameters are categorized according to primary coding:
# blood pressure, heart frequency, oxygen saturation, temperature, gcs, ph

# ============================================================================ #

#' Primary coding clinical parameters
#' 
#' adds the following columns to stv1: 
#' ecu_hpb, ecu_hf, ecu_oxy, ecu_temp, ecu_gcs, ecu_ph
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export
#' @export

primary_coding_suep_clinical_params <- function(trial_data, pid, visitid) {
  
  tryCatch(expr = {trial_data$stv1$ecu_hbp <- categorize_bloodpressure_ecu(trial_data$stv1$gec_vitals_psys, trial_data$stv1$gec_vitals_pdias)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing blood pressure did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data$stv1$ecu_hf <- categorize_heartfrequency_ecu(trial_data$stv1$gec_vitals_hf)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing heart frequency did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data$stv1$ecu_so2 <- categorize_oxigensaturation_ecu(trial_data$stv1$gec_vitals_so2)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing oxygen saturation did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data$stv1$ecu_temp <- categorize_temp_ecu(trial_data$stv1$gec_vitals_temp)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing body temperature did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data$stv1$ecu_gcs <- categorize_gcs_ecu(trial_data$stv1$vitals_gcs)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing glasgow coma scale did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data$stv1$ecu_ph <- categorize_ph_ecu(trial_data$stv1$gec_bga_ph)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing ph did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data$stv1$ecu_resp_rate <- categorize_resp_rate_ecu(trial_data$stv1$gec_vitals_resp)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() categorizing respiration rate did not work. This is likely due to missing variables.")
             print(e)})
  
  return(trial_data)
}


# Scores =======================================================================

# the following scores are categorized according to primary coding:
# Barthel Index, MoCa, EQ5D-5L, WHO-Scale

# ============================================================================ #


#' Primary coding Barthel index prior to infection (baseline)
#' 
#' adds the following column to bv1:
#' ecu_bl_barthel_cat
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_baseline_barthel <- function(trial_data, pid, visitid) {
  
  formname_to_add_vars <- "bv1"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_bl_barthel_cat = categorize_barthel_ecu(.data$bl_disab_barthel)
    )%>% 
    select(matches(visitid), .data$ecu_bl_barthel_cat)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}

#' Primary coding Barthel index (patient status)
#' 
#' adds the following column to fuv1:
#' ecu_patient_barthel_cat
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_patient_barthel <- function(trial_data, pid, visitid) {
  
  formname_to_add_vars <- "fuv1"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_patient_barthel_cat = categorize_barthel_ecu(.data$disab_barthel)
    )%>% 
    select(matches(visitid), .data$ecu_patient_barthel_cat)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding MoCA
#' 
#' adds the following column to fuv3:
#' ecu_moca_cat 
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_moca <- function(trial_data, pid, visitid) {
  
  formname_to_add_vars <- "fuv3"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_moca_cat = categorize_moca_ecu(.data$ne_moca))%>% 
    select(matches(visitid), .data$ecu_moca_cat)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-5L-Index
#' 
#' adds the following column to prom:
#' ecu_eq5d_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d5l <- function(trial_data, pid, visitid) {
  
  formname_to_add_vars <- "prom"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d5l_index = calculate_eq5d5l_index(.data$eq5d_mob, .data$eq5d_care, .data$eq5d_act, .data$eq5d_pain, .data$eq5d_anx)) %>% 
    select(matches(visitid), .data$ecu_eq5d5l_index)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding Recode Brief Resilience Scale (BRS)
#' 
#' recodes the following columns in promext: 
#' brs_2, brs_4, brs_6
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_suep_recode_brs <- function(trial_data) {
  
  trial_data[["promext"]] <- trial_data[["promext"]] %>%
    mutate(brs_2 = recode_brs(.data$brs_2),
           brs_4 = recode_brs(.data$brs_4),
           brs_6 = recode_brs(.data$brs_6))
  
  return(trial_data)
}


#' Primary coding Brief Resilience Scale (BRS)
#' 
#' adds the following columns to promext: 
#' ecu_brs_sum, ecu_brs_n, ecu_brs_total, ecu_brs_cat
#'
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_brs <- function(trial_data, visitid) {
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    rowwise() %>%
    mutate(ecu_brs_sum = calculate_brs_sum(.data$brs_1, .data$brs_2, .data$brs_3, .data$brs_4, .data$brs_5, .data$brs_6),
           ecu_brs_n = calculate_brs_n(.data$brs_1, .data$brs_2, .data$brs_3, .data$brs_4, .data$brs_5, .data$brs_6),
           ecu_brs_total = calculate_brs_total(.data$ecu_brs_sum, .data$ecu_brs_n),
           ecu_brs_cat = categorize_brs_ecu(.data$ecu_brs_total)) %>%
    ungroup() %>%
    select(matches(visitid), starts_with("ecu"))
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding Chandler Fatigue Scale (CFS/SEID)
#' 
#' adds the following columns to promext:
#' ecu_cfs_seid_1,ecu_cfs_seid_2, ecu_cfs_seid_3, ecu_cfs_seid_4, ecu_cfs_seid_5, ecu_cfs_seid_6, ecu_cfs_seid_7, ecu_cfs_seid_8, ecu_cfs_seid_9,
#' ecu_cfs_seid_10, ecu_cfs_seid_11, ecu_cfs_seid_sum, ecu_cfs_seid_cat
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_cfs_seid <- function(trial_data, visitid) {
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    rowwise() %>%
    mutate(
      ecu_cfs_seid_1 = recode_cfs_seid(.data$cfs_seid_1), 
      ecu_cfs_seid_2 = recode_cfs_seid(.data$cfs_seid_2), 
      ecu_cfs_seid_3 = recode_cfs_seid(.data$cfs_seid_3),
      ecu_cfs_seid_4 = recode_cfs_seid(.data$cfs_seid_4), 
      ecu_cfs_seid_5 = recode_cfs_seid(.data$cfs_seid_5), 
      ecu_cfs_seid_6 = recode_cfs_seid(.data$cfs_seid_6), 
      ecu_cfs_seid_7 = recode_cfs_seid(.data$cfs_seid_7), 
      ecu_cfs_seid_8 = recode_cfs_seid(.data$cfs_seid_8), 
      ecu_cfs_seid_9 = recode_cfs_seid(.data$cfs_seid_9), 
      ecu_cfs_seid_10 = recode_cfs_seid(.data$cfs_seid_10), 
      ecu_cfs_seid_11 = recode_cfs_seid(.data$cfs_seid_11), 
      ecu_cfs_seid_sum = calculate_cfs_seid_sum(.data$ecu_cfs_seid_1, .data$ecu_cfs_seid_2, .data$ecu_cfs_seid_3, .data$ecu_cfs_seid_4, 
                                                .data$ecu_cfs_seid_5, .data$ecu_cfs_seid_6, .data$ecu_cfs_seid_7, .data$ecu_cfs_seid_8, 
                                                .data$ecu_cfs_seid_9, .data$ecu_cfs_seid_10, .data$ecu_cfs_seid_11),
      ecu_cfs_seid_cat = categorize_cfs_seid(.data$ecu_cfs_seid_sum)
    ) %>%
    ungroup() %>%
    select(matches(visitid), starts_with("ecu_cfs_seid"))
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by = visitid)
  
  return(trial_data)
  
}


#' Primary coding WHO-Scale
#' 
#' adds the following dataframe to trial_data: 
#'  ecu_who_scale_per_visit_data
#'  
#' adds the following columns to scv:
#'  ecu_who_scale_max, ecu_who_scale_max.factor, ecu_who_scale_with_diag_max, ecu_who_scale_with_diag_max.factor
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_who_scale <- function(trial_data, pid, visitid) {
  
  trial_data[["ecu_who_scale_per_visit_data"]] <- build_who_scale_suep_df(trial_data, pid, visitid)
  trial_data <- summarize_who_scale(trial_data, pid)
  
  return(trial_data)
}


# SUEP Wrapper primary coding ==================================================

#' Primary coding SUEP Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#' primary_coding_suep_age, primary_coding_suep_bmi, primary_coding_suep_clinical_params, 
#' primary_coding_suep_baseline_barthel, primary_coding_suep_patient_barthel, primary_coding_suep_moca
#'
#' @param trial_data A secuTrial data object
#' @return A secuTrial data object with primary coded variables and dataframes
#' @export

primary_coding_suep <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  mnpvislabel <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  # Demographics
  tryCatch(expr = {trial_data <- primary_coding_suep_age(trial_data, pid, visitid)},
            error = function(e) {
              warning("primary_coding_suep_age() did not work. This is likely due to missing variables.")
              print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_bmi(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  # Clinical parameters
  tryCatch(expr = {trial_data <- primary_coding_suep_clinical_params(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  # Scores
  tryCatch(expr= {trial_data <- primary_coding_suep_baseline_barthel(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_baseline_barthel() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_patient_barthel(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_patient_barthel() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_moca(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_moca() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d5l(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d5l() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_recode_brs(trial_data)},
           error = function(e) {
             warning("primary_coding_suep_recode_brs() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_brs(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_brs() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_cfs_seid(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_cfs_seid() did not work. This is likely due to missing variables.")
             print(e)})
  
  #WHO-Scale
  tryCatch(expr = {trial_data <- primary_coding_suep_who_scale(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_who_scale() did not work. This is likely due to missing variables.")
             print(e)})

  catw("Primary Coding done")
  
  return(trial_data)
}


# SUEP helper functions for primary coding =====================================

## Recoding ====================================================================

#' Recode Brief Resilience Scale items
#' 
#' @description item polarization varies across BRS items 
#' item 1, 3, 5 need levels from 1 = "Do not agree at all" to 5 = "Totally agree" 
#' item 2, 4, 6 need levels from 5 = "Do not agree at all" to 1 = "Totally agree"
#' function recodes items 2, 4, 6 to needed levels if they were leveled like item 1, 3 and 5
#' 
#' @param brs BRS item, that needs recoding
#' @return A numerical vector with recoded BRS item

recode_brs <- function(brs) {
  
  brs <- as.integer(case_when(brs == 1 ~ "5", 
                              brs == 2 ~ "4",
                              brs == 3 ~ "3",
                              brs == 4 ~ "2", 
                              brs == 5 ~ "1",
                              brs == -1 ~ "-1"))
  
  return(brs)
}


#' Recode Chandler Fatigue Scale (CFS/SEID) items
#' 
#' @description recodes the Chandler Fatigue Scale (CFS/SEID) items into a dichotomous variable
#' 
#' @param cfs Item of Chandler Fatigue Scale (CFS/SEID) that needs to be recoded
#' @return A vector with the correctly coded CFS/SEID item
#' @export

recode_cfs_seid <- function(cfs) {
  
  cfs_rec <- case_when(cfs == -1 ~ -1, 
                       cfs <= 2 ~ 0,
                       cfs > 2 ~ 1)
  return(cfs_rec)
  
}


## Chandler Fatigue Scale (CFS/SEID) ==========================================

#' Calculate Chandler Fatigue Scale (CFS/SEID) sum score
#' 
#' @description Calculate sum score of CFS/SEID
#' 
#' items need to be recoded first
#' 
#' @param cfs_1 vector for item "Is fatigue a problem for you?"
#' @param cfs_2 vector for item "Do you need to rest frequently?."
#' @param cfs_3 vector for item "Do you feel tired or sleepy?"
#' @param cfs_4 vector for item "Do you have difficulty getting things done?"
#' @param cfs_5 vector for item "Do you lack energy?"
#' @param cfs_6 vector for item "Do you have less strength in your muscles?"
#' @param cfs_7 vector for item "Do you feel weak?"
#' @param cfs_8 vector for item "Do you find it difficult to concentrate?"
#' @param cfs_9 vector for item "Do you have slips of the tongue when you speak?"
#' @param cfs_10 vector for item "Do you find it difficult to think clearly?"
#' @param cfs_11 vector for item "How is your memory?"
#' 
#' @return A numeric vector with sum score of CFS/SEID
#' @export

calculate_cfs_seid_sum <- function(cfs_1, cfs_2, cfs_3, cfs_4, cfs_5, cfs_6, cfs_7, cfs_8, cfs_9, cfs_10, cfs_11) {
  
  ecu_cfs_seid_sum <- sum(ifelse(cfs_1 == -1, 0, cfs_1), 
                          ifelse(cfs_2 == -1, 0, cfs_2), 
                          ifelse(cfs_3 == -1, 0, cfs_3), 
                          ifelse(cfs_4 == -1, 0, cfs_4), 
                          ifelse(cfs_5 == -1, 0, cfs_5), 
                          ifelse(cfs_6 == -1, 0, cfs_6), 
                          ifelse(cfs_7 == -1, 0, cfs_7), 
                          ifelse(cfs_8 == -1, 0, cfs_8), 
                          ifelse(cfs_9 == -1, 0, cfs_9), 
                          ifelse(cfs_10 == -1, 0, cfs_10), 
                          ifelse(cfs_11 == -1, 0, cfs_11))
  
  return(ecu_cfs_seid_sum)
  
}


#' Categorize Chandler Fatigue Scale (CFS/SEID)
#' 
#' @description Categorize CFS/SEID
#' 
#' @param ecu_cfs_seid_sum A numerical vector with CFS/SEID sum score
#' 
#' @return A factor /w levels "No Fatigue" and "Fatigue"
#' @export

categorize_cfs_seid <- function(ecu_cfs_seid_sum) {
  
  ecu_cfs_seid_cat <- case_when(ecu_cfs_seid_sum <= 3 ~ "No fatigue",
                                ecu_cfs_seid_sum >= 4 ~ "Fatigue")
  
  return(ecu_cfs_seid_cat)
  
}


## WHO-Scale per visit =========================================================

#' Calculate and categorize WHO-Scale
#' 
#' @return dataframe, which contains the who-scale per visit
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param docid column name of document ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang :=
#' @importFrom rlang .data
#' @import lubridate
#' @export

build_who_scale_suep_df <- function(trial_data, pid, docid, visitid){
  
  # Visit Dates (one row per pat per visit) ------------------------------------
  
  mnpvislabel <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  visit_data <- trial_data$scv %>%
    full_join (trial_data$m2, by=c(pid, mnpvislabel, docid)) %>%
    full_join (trial_data$m, by=c(pid, mnpvislabel, docid)) %>%
    filter(.data$pr_visit_mode.factor != "Zus\u00e4tzliche Dokumentationsvisite" | is.na(.data$pr_visit_mode.factor)) %>%
    mutate(visit_date = coalesce(.data$gec_pr_docudate_1.date, .data$pr_docudate.date, .data$pr_incl_date.date)) %>%
    arrange(.data$visit_date) %>%
    group_by(!!sym(pid)) %>%
    mutate(!!sym(mnpvislabel) := str_replace(!!sym(mnpvislabel), "#", as.character(row_number()))) %>%
    ungroup() %>%
    select(pid, mnpvislabel, .data$visit_date)
  
  
  # Inclusion groups (one row per pat) -----------------------------------------
  # Definition von pädiatrischen Kontrollpatienten (gec_pr_inclusion = 8)
  # Einschlusskriterien für pädiatrische Kontrollgruppe, wenn eines der folgenden erfüllt ist:
  # data$scv$p_ctrl_acut_inf (andere akute Infektion) = 1 ("Ja")
  # data$scv$p_ctrl_kawa (Kawasaki-Syndrom) = 1 ("Ja")
  # data$scv$p_ctrl_mstill (Morbus Still) = 1 ("Ja")
  
  if ("p_ctrl_acut_inf" %in% names(trial_data$scv) | "p_ctrl_kawa" %in% names(trial_data$scv) |  "p_ctrl_mstill" %in% names(trial_data$scv)) {
    
    inclusion_data <- trial_data$scv %>%
      mutate(ecu_pr_inclusion = as.character(.data$gec_pr_inclusion.factor),
             ecu_pr_inclusion = 
               case_when(.data$gec_pr_inclusion == 7 & (.data$p_ctrl_acut_inf == 1 | .data$p_ctrl_kawa == 1 | .data$p_ctrl_mstill == 1) ~ "Einschluss als p\u00e4diatrische Kontrolle",
                         TRUE ~ .data$ecu_pr_inclusion)
      ) %>%
      select(pid, .data$ecu_pr_inclusion)
  }   else {
    inclusion_data <- trial_data$scv %>%
      mutate(ecu_pr_inclusion = as.character(.data$gec_pr_inclusion.factor)) %>%
      select(pid, .data$ecu_pr_inclusion)
  }
  
  # Main Diagnosis (one row per pat) -------------------------------------------
  # check if main diagnosis is covid 19 (ICD10-Code: U07.1 or U07.2)
  # coded in eresid$resid_icd10 and eresid$p_hospital (for paediatric patients)
  
  trial_data$eresid$resid_icd10 <- str_replace(trial_data$eresid$resid_icd10, " ", "")
  comparable_covid_icd <- str_detect(trial_data$eresid$resid_icd10, regex("U(07|7|\\.07|\\.7)(\\.|,|:|\\-|\\.0|\\.)(1|2)", ignore_case = TRUE))
  
  main_diag_data <- trial_data$eresid %>%
    mutate(
      ecu_main_diag_covid = case_when(comparable_covid_icd | .data$p_hospital == 1 ~ "Hauptdiagnose Covid",
                                      (!is.na(.data$resid_icd10) & !comparable_covid_icd) | .data$p_hospital == 0 ~ "Hauptdiagnose Andere",
                                      .data$resid_icd10_uk < 0 | .data$p_hospital < 0 ~ coalesce(as.character(.data$resid_icd10_uk.factor), as.character(.data$p_hospital.factor)))) %>%
    # reduce to one row per pat
    group_by(!!sym(pid)) %>%  
    summarise(ecu_main_diag_covid = case_when(any(.data$ecu_main_diag_covid == "Hauptdiagnose Covid") ~ "Hauptdiagnose Covid",
                                              any(.data$ecu_main_diag_covid == "Hauptdiagnose Andere") ~ "Hauptdiagnose Andere",
                                              TRUE ~ first(.data$ecu_main_diag_covid))) %>%
    ungroup() %>%
    select(pid, .data$ecu_main_diag_covid)
  
  
  # Date of last Treatment Update ----------------------------------------------
  
  treatment_update_date <- trial_data$fuv3 %>%
    left_join (trial_data$vp %>% select (.data$mnpvisid, mnpvislabel), by = "mnpvisid") %>%
    filter (.data$pr_check_treat.factor == "Ja") %>% #only treatment_update == YES
    rename (treatment_update_visit = mnpvislabel,
            treatment_update.datetime = .data$fuv3_date.date) %>%
    group_by (!!sym(pid)) %>%
    slice_max (.data$treatment_update.datetime, with_ties = FALSE) %>%
    ungroup() %>%
    mutate (treatment_update.date = as_date(.data$treatment_update.datetime)) %>%
    select (pid, .data$treatment_update_visit, .data$treatment_update.date) 
  
  
  # Oxygenation (multiple rows per pat) ----------------------------------------
  
  oxy_data <- trial_data$fv6 %>%
    left_join(trial_data$eoxy, by = pid) %>%
    left_join(treatment_update_date, by = pid) %>%
    mutate(
      gec_oxy_end.date = case_when(.data$gec_oxy_end_on.factor =="Andauernd" ~ .data$treatment_update.date,
                                   .data$gec_oxy_end_d.factor == "Auf Monat genau" ~ rollforward(.data$gec_oxy_end.date, roll_to_first = FALSE),
                                   .data$gec_oxy_end_d.factor == "Auf Woche genau" ~ ceiling_date(.data$gec_oxy_end.date, unit = "week", week_start = getOption ("lubridate.week.start", 1)),
                                   TRUE ~ .data$gec_oxy_end.date),
      gec_oxy_start.date = case_when (.data$gec_oxy_start_d.factor == "Auf Monat genau" ~ rollback(.data$gec_oxy_start.date, roll_to_first = TRUE),
                                      .data$gec_oxy_start_d.factor == "Auf Woche genau" ~ floor_date(.data$gec_oxy_start.date, unit = "week", week_start = getOption ("lubridate.week.start", 1)),
                                      TRUE ~ .data$gec_oxy_start.date),
      ecu_oxy_interval = interval(.data$gec_oxy_start.date, .data$gec_oxy_end.date),
      gec_oxy_type.factor = factor(fct_reorder(.data$gec_oxy_type.factor, .data$gec_oxy_type, na.rm = FALSE), ordered = TRUE)) %>%
    select(pid, .data$gec_oxy.factor, .data$gec_oxy_type, .data$gec_oxy_type.factor, .data$gec_oxy_start.date, .data$gec_oxy_start_d.factor, .data$gec_oxy_start_uk.factor, 
           .data$gec_oxy_end.date, .data$gec_oxy_end_d.factor, .data$gec_oxy_end_uk.factor, .data$gec_oxy_end_on.factor, .data$treatment_update.date, 
           .data$ecu_oxy_interval) #%>%
  #group_by(!!sym(pid), .data$gec_oxy) # %>%
  # nest(oxy_data = -c(!!sym(pid), .data$gec_oxy)) 
  
  
  # Hospitalisation (one row per pat per start date) ---------------------------
  
  hospital_data <- trial_data$eward %>%
    bind_rows(trial_data$eresid) %>% 
    bind_rows(treatment_update_date) %>%
    mutate(
      ecu_ward = coalesce(.data$gec_ward.factor, .data$resid.factor),
      ecu_ward_resid_start.date = coalesce(.data$ward_start.date, .data$resid_start.date), 
      ecu_ward_resid_start_d.factor = coalesce(.data$ward_start_d.factor, .data$resid_start_d.factor), 
      ecu_ward_resid_start_uk.date = coalesce(.data$ward_start_uk.factor, .data$resid_start_uk.factor), 
      ecu_ward_resid_start.date = case_when(.data$ecu_ward_resid_start_d.factor == "Auf Monat genau" ~ rollback(.data$ecu_ward_resid_start.date, roll_to_first = TRUE),
                                            .data$ecu_ward_resid_start_d.factor == "Auf Woche genau" ~ floor_date(.data$ecu_ward_resid_start.date, unit = "week", week_start = getOption ("lubridate.week.start", 1)),
                                            TRUE ~ .data$ecu_ward_resid_start.date),
      ecu_ward_resid_end.date = coalesce(.data$ward_end.date, .data$resid_end.date), 
      ecu_ward_resid_end_d.factor = coalesce(.data$ward_end_d.factor, .data$resid_end_d.factor), 
      ecu_ward_resid_end_uk.date = coalesce(.data$ward_end_uk.factor, .data$resid_end_uk.factor), 
      ecu_ward_resid_end_on.date = coalesce(.data$ward_end_on.factor, .data$resid_end_on.factor), 
      ecu_ward_resid_end.date = case_when(.data$ecu_ward_resid_end_on.date == "Andauernd" ~ .data$treatment_update.date,
                                          .data$ecu_ward_resid_end_d.factor == "Auf Monat genau" ~ rollforward(.data$ecu_ward_resid_end.date, roll_to_first = FALSE),
                                          .data$ecu_ward_resid_end_d.factor == "Auf Woche genau" ~ ceiling_date(.data$ecu_ward_resid_end.date, unit = "week", week_start = getOption ("lubridate.week.start", 1)),
                                          TRUE ~ .data$ecu_ward_resid_end.date),
      ecu_hospital_interval = interval(.data$ecu_ward_resid_start.date, .data$ecu_ward_resid_end.date)) %>%
    filter(str_detect(tolower(.data$ecu_ward), "station|krankenhaus|keine informationen") & (.data$ecu_ward_resid_start.date >= ymd(20200101) | is.na(.data$ecu_ward_resid_start.date))) %>% # anything before 2020 must be covid19 unrelated
    full_join(trial_data$fv1, by=pid) %>%
    select(pid, .data$ward.factor, starts_with("ecu"), .data$gec_resid_disch.factor)
  

  # Death (one row per pat) ----------------------------------------------------
  
  eresid_death <- trial_data$eresid %>%
    filter(.data$gec_resid_disch.factor == "Tod") %>%
    rename(ecu_death_date.date = .data$resid_end.date,
           ecu_death_date_d.factor = .data$resid_end_d.factor,
           ecu_death_date_uk.factor = .data$resid_end_uk.factor) %>%
    select(pid, .data$ecu_death_date.date, .data$ecu_death_date_d.factor, .data$ecu_death_date_uk.factor)
  
  tryCatch(expr = {fv2_6_death <- trial_data$fv2_6 %>%
    filter(.data$gec_death.factor == "Ja" | .data$gec_death.factor == "Keine Informationen verf\u00fcgbar") %>%
    rename(ecu_death_date.date = .data$death_date.date,
           ecu_death_date_d.factor = .data$death_date_d.factor,
           ecu_death_date_uk.factor = .data$death_date_uk.factor) %>%
    select(pid, .data$ecu_death_date.date, .data$ecu_death_date_d.factor, .data$ecu_death_date_uk.factor)},
    error = function(e) {
      warning("death_date is missing in fv2_6")
             print(e)})
  
  death_data <- trial_data$fv2_6_death %>%
    bind_rows(eresid_death) %>%
    group_by(!!sym(pid)) %>%
    mutate(
      n = n(),
      are_death_dates_same = length(unique(.data$ecu_death_date.date)) == 1) %>%
    # Select first death with exct date if there is more than one
    filter(if (any(n > 1 & .data$are_death_dates_same)) .data$ecu_death_date_d.factor == "Exakte Angabe" else TRUE) %>% 
    # after that the min date
    slice_min(.data$ecu_death_date.date, with_ties = FALSE) %>% 
    ungroup() %>%
    select(pid, .data$ecu_death_date.date, .data$ecu_death_date_d.factor, .data$ecu_death_date_uk.factor)
  
  
  # WHO Scale Data  (one row per pat per visit) --------------------------------
  
  who_scale_per_visit_data <- visit_data %>%
    full_join(select(trial_data$fv15, pid, .data$pr_end_date.date, .data$pr_end_reason.factor), by=pid) %>%
    full_join(inclusion_data, by =pid) %>%
    full_join(main_diag_data, by =pid) %>%
    full_join(death_data, by=pid) %>%
    full_join(oxy_data, by=pid) %>%
    full_join(hospital_data, by=pid) %>%
    group_by(!!sym(pid),!!sym(mnpvislabel)) %>%
    mutate(
      is_hospital = case_when(.data$visit_date %within% .data$ecu_hospital_interval ~ 1,
                              .data$ward.factor == "Keine Informationen verf\u00fcgbar" ~ -1,
                              !(.data$visit_date %within% .data$ecu_hospital_interval) | .data$ward.factor == "Nein" ~ 0),
      is_oxy = case_when(.data$visit_date %within% .data$ecu_oxy_interval ~ 1,
                         .data$gec_oxy.factor == "Keine Informationen verf\u00fcgbar" ~ -1,
                         !(.data$visit_date %within% .data$ecu_oxy_interval) | .data$gec_oxy.factor == "Nein" ~ 0),
      oxy_hospital_interval_overlap = int_overlaps(.data$ecu_oxy_interval, .data$ecu_hospital_interval),
      is_oxy_type_severe = case_when(oxy_hospital_interval_overlap == TRUE & .data$gec_oxy_type >= "2" ~ TRUE,
                                     TRUE ~ FALSE)
    ) %>%
    mutate(
      ecu_who_scale.factor = case_when(str_detect(.data$ecu_pr_inclusion, "Kontroll") ~ "Kontrollgruppe, ohne Sars-Infektion",
                                       .data$ward.factor == "Nein" ~ "Ambulant, milde Phase", # ward == 0 = "Nein" 
                                       .data$ward.factor == "Keine Informationen verf\u00fcgbar" | 
                                         .data$gec_oxy.factor == "Keine Informationen verf\u00fcgbar" ~ "Keine Informationen verf\u00fcgbar",
                                       .data$is_hospital == 0 ~ "Ambulant, milde Phase",
                                       .data$visit_date >= .data$ecu_death_date.date ~ "Verstorben",
                                       .data$is_oxy_type_severe & .data$is_hospital == 1 ~ "Hospitalisiert, schwere Phase",
                                       .data$is_hospital == 1  & .data$is_oxy != "-1" ~ "Hospitalisiert, moderate Phase"),
      ecu_who_scale = as.integer(case_when(.data$ecu_who_scale.factor == "Keine Informationen verf\u00fcgbar" ~ -1,
                                           .data$ecu_who_scale.factor == "Kontrollgruppe, ohne Sars-Infektion" ~ 0,
                                           .data$ecu_who_scale.factor == "Ambulant, milde Phase" ~ 1,
                                           .data$ecu_who_scale.factor == "Hospitalisiert, moderate Phase" ~ 2,
                                           .data$ecu_who_scale.factor == "Hospitalisiert, schwere Phase" ~ 3,
                                           .data$ecu_who_scale.factor == "Verstorben" ~ 4)),
      ecu_who_scale_with_diag.factor = case_when(ecu_who_scale.factor == "Hospitalisiert, moderate Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Covid" ~ "Hospitalisiert wegen Covid, moderate Phase",
                                                 ecu_who_scale.factor == "Hospitalisiert, moderate Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Andere" ~ "Hospitalisiert mit Covid, moderate Phase",
                                                 ecu_who_scale.factor == "Hospitalisiert, schwere Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Covid" ~ "Hospitalisiert wegen Covid, schwere Phase",
                                                 ecu_who_scale.factor == "Hospitalisiert, schwere Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Andere" ~ "Hospitalisiert mit Covid, schwere Phase",
                                                 is.na(.data$ecu_main_diag_covid) ~ NA,
                                                 .data$ecu_main_diag_covid == "Keine Informationen verf\u00fcgbar" ~ "Keine Informationen verf\u00fcgbar",
                                                 TRUE ~ ecu_who_scale.factor),
      ecu_who_scale_with_diag = as.integer(case_when(.data$ecu_who_scale_with_diag.factor == "Keine Informationen verf\u00fcgbar" ~ -1,
                                                     .data$ecu_who_scale_with_diag.factor == "Kontrollgruppe, ohne Sars-Infektion" ~ 0,
                                                     .data$ecu_who_scale_with_diag.factor == "Ambulant, milde Phase" ~ 1,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert mit Covid, moderate Phase" ~ 2,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert wegen Covid, moderate Phase" ~ 3,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert mit Covid, schwere Phase" ~ 4,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert wegen Covid, schwere Phase" ~ 5,
                                                     .data$ecu_who_scale_with_diag.factor == "Verstorben" ~ 6))
    ) %>%
    ungroup()
  
  return(who_scale_per_visit_data)
  
}

## WHO-Scale max per patient ===================================================

#' Summarize WHO-Scale
#' 
#' @return a vecctor, which contains the maximum who-scale per patient
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @importFrom rlang .data
#' @export

summarize_who_scale <- function(trial_data, pid) {
  
  who_scale_max <- trial_data[["ecu_who_scale_per_visit_data"]] %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$ecu_who_scale) %>% 
    rename(ecu_who_scale_max = .data$ecu_who_scale,
           ecu_who_scale_max.factor = .data$ecu_who_scale.factor) %>%
    select(!!sym(pid), .data$ecu_who_scale_max, .data$ecu_who_scale_max.factor) %>%
    distinct() %>%
    ungroup()
  
  who_scale_with_diag_max <- trial_data[["ecu_who_scale_per_visit_data"]] %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$ecu_who_scale_with_diag) %>%
    rename(ecu_who_scale_with_diag_max = .data$ecu_who_scale_with_diag,
           ecu_who_scale_with_diag_max.factor = .data$ecu_who_scale_with_diag.factor) %>%
    select(!!sym(pid), .data$ecu_who_scale_with_diag_max, .data$ecu_who_scale_with_diag_max.factor) %>%
    distinct() %>%
    ungroup()
  
  trial_data[["scv"]] <- trial_data[["scv"]] %>%
    left_join(who_scale_max, by = pid) %>%
    left_join(who_scale_with_diag_max, by = pid)
  
  return(trial_data)
}


