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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
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
    select(matches(visitid), "ecu_age", "ecu_age_cat_dec", "ecu_age_cat_3") %>%
    labelled::remove_var_label()
  
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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
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
    select(matches(visitid), "ecu_bmi", "ecu_bmi_est", "ecu_bmi_cat", "ecu_bmi_est_cat") %>%
    labelled::remove_var_label()
  
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
# Barthel Index, MoCa, EQ5D-5L, WHO-Scale, Post-COVID-Score

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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "bv1"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_bl_barthel_cat = categorize_barthel_ecu(.data$bl_disab_barthel)
    )%>% 
    select(matches(visitid), "ecu_bl_barthel_cat") %>%
    labelled::remove_var_label()
  
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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "fuv1"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_patient_barthel_cat = categorize_barthel_ecu(.data$disab_barthel)
    )%>% 
    select(matches(visitid), "ecu_patient_barthel_cat") %>% 
    labelled::remove_var_label()
  
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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "fuv3"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_moca_cat = categorize_moca_ecu(.data$ne_moca))%>% 
    select(matches(visitid), "ecu_moca_cat") %>% 
    labelled::remove_var_label()
  
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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "prom"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d5l_index = calculate_eq5d5l_index(.data$eq5d_mob, .data$eq5d_care, .data$eq5d_act, .data$eq5d_pain, .data$eq5d_anx)) %>% 
    select(matches(visitid), "ecu_eq5d5l_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 4 (PROXY Version)
#' 
#' adds the following column to promp4:
#' ecu_eq5d3l_proxy_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_4p <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp4"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_proxy_index = calculate_eq5d3l_proxy_index(.data$p_prom4_eq5d_par1, .data$p_prom4_eq5d_par2, .data$p_prom4_eq5d_par3, .data$p_prom4_eq5d_par4,
                                                                 .data$p_prom4_eq5d_par5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_proxy_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 5-7 (PROXY Version)
#' 
#' adds the following column to promp5:
#' ecu_eq5d3l_proxy_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_5t7p <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp5"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_proxy_index = calculate_eq5d3l_proxy_index(.data$p_prom5_eq5d_par1, .data$p_prom5_eq5d_par2, .data$p_prom5_eq5d_par3, .data$p_prom5_eq5d_par4,
                                                                 .data$p_prom5_eq5d_par5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_proxy_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 8 (PROXY Version)
#' 
#' adds the following column to promp8:
#' ecu_eq5d3l_proxy_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_8p <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp8"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_proxy_index = calculate_eq5d3l_proxy_index(.data$p_prom8_eq5d_par1, .data$p_prom8_eq5d_par2, .data$p_prom8_eq5d_par3, .data$p_prom8_eq5d_par4,
                                                                 .data$p_prom8_eq5d_par5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_proxy_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 8 (Y Version)
#' 
#' adds the following column to promp8k:
#' ecu_eq5d3l_y_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_8y <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp8k"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_y_index = calculate_eq5d3l_y_index(.data$p_prom8_eq5d_kid1, .data$p_prom8_eq5d_kid2, .data$p_prom8_eq5d_kid3, .data$p_prom8_eq5d_kid4,
                                                         .data$p_prom8_eq5d_kid5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_y_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 9-11 (PROXY Version)
#' 
#' adds the following column to promp9:
#' ecu_eq5d3l_proxy_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_9t11p <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp9"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_proxy_index = calculate_eq5d3l_proxy_index(.data$p_prom9_eq5d_par1, .data$p_prom9_eq5d_par2, .data$p_prom9_eq5d_par3, .data$p_prom9_eq5d_par4,
                                                                 .data$p_prom9_eq5d_par5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_proxy_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 9-11 (Y Version)
#' 
#' adds the following column to promp9k:
#' ecu_eq5d3l_y_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_9t11y <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp9k"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_y_index = calculate_eq5d3l_y_index(.data$p_prom9_eq5d_kid1, .data$p_prom9_eq5d_kid2, .data$p_prom9_eq5d_kid3, .data$p_prom9_eq5d_kid4,
                                                         .data$p_prom9_eq5d_kid5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_y_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-5L-Index for children aged 12 (PROXY Version)
#' 
#' adds the following column to promp12:
#' ecu_eq5d5l_proxy_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d5l_12p <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp12"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d5l_proxy_index = calculate_eq5d5l_index(.data$p_prom12_eq5d_par1, .data$p_prom12_eq5d_par2, .data$p_prom12_eq5d_par3, .data$p_prom12_eq5d_par4,
                                                           .data$p_prom12_eq5d_par5)) %>% 
    select(matches(visitid), "ecu_eq5d5l_proxy_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-5L-Index for children aged 12 (Y Version)
#' 
#' adds the following column to promp12k:
#' ecu_eq5d5l_y_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d5l_12y <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp12k"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d5l_y_index = calculate_eq5d5l_y_index(.data$p_prom12_eq5d_t_kid1, .data$p_prom12_eq5d_t_kid2, .data$p_prom12_eq5d_t_kid3, 
                                                         .data$p_prom12_eq5d_t_kid4, .data$p_prom12_eq5d_t_kid5)) %>% 
    select(matches(visitid), "ecu_eq5d5l_y_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-3L-Index for children aged 13-18 (PROXY Version)
#' 
#' adds the following column to promp13:
#' ecu_eq5d3l_proxy_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d3l_13t18p <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp13"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d3l_proxy_index = calculate_eq5d3l_proxy_index(.data$p_prom13_eq5d_p1, .data$p_prom13_eq5d_p2, .data$p_prom13_eq5d_p3, .data$p_prom13_eq5d_p4,
                                                                 .data$p_prom13_eq5d_p5)) %>% 
    select(matches(visitid), "ecu_eq5d3l_proxy_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding EQ5D-5L-Index for children aged 13-18 (Y Version)
#' 
#' adds the following column to promp13k:
#' ecu_eq5d5l_y_index
#'
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom eq5d eq5d
#' @importFrom rlang .data
#' @export

primary_coding_suep_eq5d5l_13t18y <- function(trial_data, pid, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promp13k"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_eq5d5l_y_index = calculate_eq5d5l_y_index(.data$p_prom13_eq5d_t_kid1, .data$p_prom13_eq5d_t_kid2, .data$p_prom13_eq5d_t_kid3, 
                                                         .data$p_prom13_eq5d_t_kid4, .data$p_prom13_eq5d_t_kid5)) %>% 
    select(matches(visitid), "ecu_eq5d5l_y_index") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding Brief Resilience Scale (BRS)
#' 
#' adds the following columns to promext: 
#' ecu_brs_2, ecu_brs_4, ecu_brs_6 (recoded BRS items)
#' ecu_brs_sum, ecu_brs_n, ecu_brs_total, ecu_brs_cat
#'
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_brs <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_brs_2 = recode_brs(.data$brs_2.factor),
           ecu_brs_4 = recode_brs(.data$brs_4.factor),
           ecu_brs_6 = recode_brs(.data$brs_6.factor),
           ecu_brs_sum = calculate_brs_sum(.data$brs_1, .data$ecu_brs_2, .data$brs_3, .data$ecu_brs_4, .data$brs_5, .data$ecu_brs_6),
           ecu_brs_n = calculate_brs_n(.data$brs_1, .data$ecu_brs_2, .data$brs_3, .data$ecu_brs_4, .data$brs_5, .data$ecu_brs_6),
           ecu_brs_total = calculate_brs_total(.data$ecu_brs_sum, .data$ecu_brs_n),
           ecu_brs_cat = categorize_brs_ecu(.data$ecu_brs_total)) %>%
    select(matches(visitid), contains("ecu_brs")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding Six Item Loneliness Scale (6 ILS)
#' 
#' adds the following column to promext: 
#' ecu_six_ils1,ecu_six_ils2, ecu_six_ils3, ecu_six_ils4, ecu_six_ils5, ecu_six_ils6, ecu_six_ils_total, ecu_six_ils_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_suep_6ils <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_six_ils1 = recode_6ils(.data$lonely_1.factor, version = "neg"),
           ecu_six_ils2 = recode_6ils(.data$lonely_2.factor, version = "pos"),
           ecu_six_ils3 = recode_6ils(.data$lonely_3.factor, version = "pos"),
           ecu_six_ils4 = recode_6ils(.data$lonely_4.factor, version = "neg"),
           ecu_six_ils5 = recode_6ils(.data$lonely_5.factor, version = "pos"),
           ecu_six_ils6 = recode_6ils(.data$lonely_6.factor, version = "neg"),
           ecu_six_ils_total = calculate_6ils_total(.data$ecu_six_ils1, .data$ecu_six_ils2, .data$ecu_six_ils3, .data$ecu_six_ils4, .data$ecu_six_ils5, .data$ecu_six_ils6),
           ecu_six_ils_cat = categorize_6ils_ecu(.data$ecu_six_ils_total)) %>%
    select(matches(visitid), contains("ecu_six_ils")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding Perceived Stress Scale (PSS)
#' 
#' adds the following column to promext: 
#' ecu_six_ils1,ecu_six_ils2, ecu_six_ils3, ecu_six_ils4, ecu_six_ils5, ecu_six_ils6, ecu_six_ils_total, ecu_six_ils_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_suep_pss <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_pss1 = recode_pss(.data$pss_1.factor, version = 1),
           ecu_pss2 = recode_pss(.data$pss_2.factor, version = 2),
           ecu_pss3 = recode_pss(.data$pss_3.factor, version = 2),
           ecu_pss4 = recode_pss(.data$pss_4.factor, version = 1),
           ecu_pss_total = calculate_pss_total(.data$ecu_pss1, .data$ecu_pss2, .data$ecu_pss3, .data$ecu_pss4, pss5 = 0, pss6 = 0, pss7 = 0, pss8 = 0, pss9 = 0, pss10 = 0)) %>%
    select(matches(visitid), contains("ecu_pss")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding Chalder Fatigue Scale (CFQ-11)
#' 
#' adds the following columns to promext:
#' ecu_cfq11_1,ecu_cfq11_2, ecu_cfq11_3, ecu_cfq11_4, ecu_cfq11_5, ecu_cfq11_6, ecu_cfq11_7, ecu_cfq11_8, ecu_cfq11_9,
#' ecu_cfq11_10, ecu_cfq11_11, ecu_cfq11_sum, ecu_cfq11_cat
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_cfq11 <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_cfq11_1 = recode_cfq11(.data$cfs_seid_1), 
           ecu_cfq11_2 = recode_cfq11(.data$cfs_seid_2), 
           ecu_cfq11_3 = recode_cfq11(.data$cfs_seid_3),
           ecu_cfq11_4 = recode_cfq11(.data$cfs_seid_4), 
           ecu_cfq11_5 = recode_cfq11(.data$cfs_seid_5), 
           ecu_cfq11_6 = recode_cfq11(.data$cfs_seid_6), 
           ecu_cfq11_7 = recode_cfq11(.data$cfs_seid_7), 
           ecu_cfq11_8 = recode_cfq11(.data$cfs_seid_8), 
           ecu_cfq11_9 = recode_cfq11(.data$cfs_seid_9), 
           ecu_cfq11_10 = recode_cfq11(.data$cfs_seid_10), 
           ecu_cfq11_11 = recode_cfq11(.data$cfs_seid_11), 
           ecu_cfq11_sum = calculate_cfq11_sum(.data$ecu_cfq11_1, .data$ecu_cfq11_2, .data$ecu_cfq11_3, .data$ecu_cfq11_4, 
                                               .data$ecu_cfq11_5, .data$ecu_cfq11_6, .data$ecu_cfq11_7, .data$ecu_cfq11_8, 
                                               .data$ecu_cfq11_9, .data$ecu_cfq11_10, .data$ecu_cfq11_11),
           ecu_cfq11_cat = categorize_cfq11(.data$ecu_cfq11_sum)
    ) %>%
    select(matches(visitid), contains("ecu_cfq11")) %>% 
    labelled::remove_var_label()
  
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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid
  visitid <- trial_data$export_options$id_names$visitid
  
  trial_data[["ecu_who_scale_per_visit_data"]] <- build_who_scale_suep_df(trial_data, pid, visitid) %>% 
    labelled::remove_var_label()
  trial_data <- summarize_who_scale(trial_data, pid)
  
  return(trial_data)
}


#' Primary coding PCS-Score
#' 
#' adds the following dataframes to data: 
#' ecu_long_symptom_data, ecu_pcs_score
#'
#' @param trial_data A secuTrial data object
#' @param prom A vector indication whether proms should be included in pcs calculation or not; needs to be specified as prom = "No" or prom = "Yes"
#' @return A secuTrial data object with primary coded variables and dataframes
#' @export

primary_coding_suep_pcs_score <- function(trial_data, prom = "No") {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  trial_data[["ecu_long_symptom_data"]] <- build_suep_long_symptom_df(trial_data, pid)
  
  if (prom == "No") {trial_data[["ecu_pcs_score"]] <- build_pcs_score_suep_df_without_proms(trial_data, pid) %>% 
    labelled::remove_var_label()}
  else if (prom == "Yes") {trial_data[["ecu_pcs_score"]] <- build_pcs_score_suep_df_with_proms(trial_data, pid) %>% 
    labelled::remove_var_label()}
  
  return(trial_data)
}


#' Primary coding Patient Health Questionnaire (PHQ-4)
#' 
#' adds the following column to prom: 
#' ecu_phq4_1, ecu_phq4_2, ecu_phq4_3, ecu_phq4_4, ecu_phq4_sum, ecu_phq4_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_suep_phq4 <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "prom"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_phq4_1 = recode_phq(.data$phq4_1.factor),
           ecu_phq4_2 = recode_phq(.data$phq4_2.factor),
           ecu_phq4_3 = recode_phq(.data$phq4_3.factor),
           ecu_phq4_4 = recode_phq(.data$phq4_4.factor),
           ecu_phq4_sum = calculate_phq4_sum(.data$ecu_phq4_1, .data$ecu_phq4_2, .data$ecu_phq4_3, .data$ecu_phq4_4),
           ecu_phq4_cat = categorize_phq4_ecu(.data$ecu_phq4_sum)) %>%
    select(matches(visitid), contains("ecu_phq4")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


#' Primary coding Pain diagnostic Questionnaire (DN-4 - Interview)
#' 
#' adds the following column to promext: 
#' ecu_dn4_sum, ecu_dn4_cat
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @export

primary_coding_suep_dn4 <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_dn4_1 = recode_dn4(.data$pain_dn2_1),
           ecu_dn4_2 = recode_dn4(.data$pain_dn2_2),
           ecu_dn4_3 = recode_dn4(.data$pain_dn2_3),
           ecu_dn4_4 = recode_dn4(.data$pain_dn2_4),
           ecu_dn4_5 = recode_dn4(.data$pain_dn2_5),
           ecu_dn4_6 = recode_dn4(.data$pain_dn2_6),
           ecu_dn4_7 = recode_dn4(.data$pain_dn2_7),
           ecu_dn4_sum = calculate_dn4_sum(.data$ecu_dn4_1, .data$ecu_dn4_2, .data$ecu_dn4_3, .data$ecu_dn4_4, .data$ecu_dn4_5, .data$ecu_dn4_6, .data$ecu_dn4_7),
           ecu_dn4_cat = categorize_dn4_ecu(.data$ecu_dn4_sum)) %>%
    select(matches(visitid), contains("ecu_dn4")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


# ARDS =========================================================================

#' Primary coding for ARDS by imaging procedures
#' 
#' adds the following dataframes to data: 
#' ecu_ards
#' 
#' @param trial_data A secuTrial data object
#' @return A secuTrial data object with primary coded variables and dataframes
#' @export

primary_coding_suep_ards <- function(trial_data) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  
  trial_data[["ecu_ards"]] <- build_ards_df(trial_data, pid) %>% 
    labelled::remove_var_label()
  
  return(trial_data)
}


# Recode PROMIS ================================================================

#' Primary coding PROMIS-29 Dyspnoe items (recoding)
#'  
#' adds the following columns to promext: 
#' ecu_pro_dysp_1, ecu_pro_dysp_2, ecu_pro_dysp_3, ecu_pro_dysp_4, ecu_pro_dysp_5, ecu_pro_dysp_6, ecu_pro_dysp_7,
#' ecu_pro_dysp_8, ecu_pro_dysp_9, ecu_pro_dysp_10
#'
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @return A secuTrial data object with primary coded variables and dataframes
#' @export

primary_coding_suep_recode_promis_dysp <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_pro_dysp_1 = recode_promis_dyspnoe(.data$pro_dysfl001.factor), 
           ecu_pro_dysp_2 = recode_promis_dyspnoe(.data$pro_dysfl002.factor),
           ecu_pro_dysp_3 = recode_promis_dyspnoe(.data$pro_dysfl003.factor),
           ecu_pro_dysp_4 = recode_promis_dyspnoe(.data$pro_dysfl004.factor),
           ecu_pro_dysp_5 = recode_promis_dyspnoe(.data$pro_dysfl005.factor),
           ecu_pro_dysp_6 = recode_promis_dyspnoe(.data$pro_dysfl006.factor),
           ecu_pro_dysp_7 = recode_promis_dyspnoe(.data$pro_dysfl007.factor),
           ecu_pro_dysp_8 = recode_promis_dyspnoe(.data$pro_dysfl008.factor),
           ecu_pro_dysp_9 = recode_promis_dyspnoe(.data$pro_dysfl009.factor),
           ecu_pro_dysp_10 = recode_promis_dyspnoe(.data$pro_dysfl10.factor)) %>%
    select(matches(visitid), contains("ecu_pro_dysp")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
}


#' Primary coding PROMIS-29 Cognitive impairments items (recoding)
#' 
#' adds the following columns to prom:
#' ecu_pro_cogn_1, ecu_pro_cogn_2, ecu_pro_cogn_3, ecu_pro_cogn_4
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_recode_promis_cogn_funct <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "prom"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_pro_cogn_1 = recode_promis_cognitive(.data$pro_pc2r.factor), 
           ecu_pro_cogn_2 = recode_promis_cognitive(.data$pro_pc35r.factor),
           ecu_pro_cogn_3 = recode_promis_cognitive(.data$pro_pc36r.factor),
           ecu_pro_cogn_4 = recode_promis_cognitive(.data$pro_pc42r.factor)) %>%
    select(matches(visitid), contains("ecu_pro_cogn")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


#' Primary coding PROMIS-29 Sleep disturbance items (recoding)
#' 
#' adds the following columns to promext:
#' ecu_pro29_sleep_1, ecu_pro29_sleep_2
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_recode_promis_sleep <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_pro29_sleep_1 = recode_promis29_sleep(.data$pro_29_sleep109.factor), 
           ecu_pro29_sleep_2 = recode_promis29_sleep(.data$pro_29_sleep116.factor)) %>%
    select(matches(visitid), "ecu_pro29_sleep_1", "ecu_pro29_sleep_2") %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


#' Primary coding PROMIS-29 Physical Function items (recoding)
#' 
#' adds the following columns to promext:
#' ecu_pro29_phys_funct_1, ecu_pro29_phys_funct_2, ecu_pro29_phys_funct_3, ecu_pro29_phys_funct_4
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_recode_promis_phys <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_pro29_phys_funct_1 = recode_promis29_phys_funct(.data$pro_29_pfa11.factor), 
           ecu_pro29_phys_funct_2 = recode_promis29_phys_funct(.data$pro_29_pfa21.factor),
           ecu_pro29_phys_funct_3 = recode_promis29_phys_funct(.data$pro_29_pfa23.factor),
           ecu_pro29_phys_funct_4 = recode_promis29_phys_funct(.data$pro_29_pfa53.factor)) %>%
    select(matches(visitid), contains("ecu_pro29_phys_funct")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


#' Primary coding PROMIS-29 Ability to participate in social roles and activities items (recoding)
#' 
#' adds the following columns to promext:
#' ecu_pro29_social_1, ecu_pro29_social_2, ecu_pro29_social_3, ecu_pro29_social_4
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export

primary_coding_suep_recode_promis_social <- function(trial_data, visitid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  visitid <- trial_data$export_options$id_names$visitid
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_pro29_social_1 = recode_promis29_social(.data$pro_29_srpper11_caps.factor), 
           ecu_pro29_social_2 = recode_promis29_social(.data$pro_29_srpper18_caps.factor),
           ecu_pro29_social_3 = recode_promis29_social(.data$pro_29_srpper23_caps.factor),
           ecu_pro29_social_4 = recode_promis29_social(.data$pro_29_srpper46_caps.factor)) %>%
    select(matches(visitid), contains("ecu_pro29_social")) %>% 
    labelled::remove_var_label()
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
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
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  ## Demographics ==============================================================
  ### Age ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_age(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_age() did not work. This is likely due to missing variables.")
             print(e)})
  
  ### BMI ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_bmi(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Clinical parameters =======================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_clinical_params(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_clinical_params() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Scores ====================================================================
  ### Barthel-Index ============================================================
  tryCatch(expr= {trial_data <- primary_coding_suep_baseline_barthel(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_baseline_barthel() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_patient_barthel(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_patient_barthel() did not work. This is likely due to missing variables.")
             print(e)})
  ### MoCA =====================================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_moca(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_moca() did not work. This is likely due to missing variables.")
             print(e)})
  ### EQ-5D-5L =================================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d5l(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d5l() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_4p(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_4p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_5t7p(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_5t7p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_8p(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_8p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_8y(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_8y() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_9t11p(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_9t11p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_9t11y(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_9t11y() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d5l_12p(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d5l_12p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d5l_12y(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d5l_12y() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d3l_13t18p(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d3l_13t18p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_eq5d5l_13t18y(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_eq5d5l_13t18p() did not work. This is likely due to missing variables or no inclusion of paediatric patients.")
             print(e)})
  ### Brief Resilience Scale ===================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_brs(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_brs() did not work. This is likely due to missing variables.")
             print(e)})
  ### Chalder Fatigue Scale ====================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_cfq11(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_cfq11() did not work. This is likely due to missing variables.")
             print(e)})
  ### 6 Item Loneliness Scale ==================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_6ils(trial_data)},
           error = function(e) {
             warning("primary_coding_suep_6ils() did not work. This is likely due to missing variables.")
             print(e)})
  ### Perceived Stress Scale ===================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_pss(trial_data)},
           error = function(e) {
             warning("primary_coding_suep_pss() did not work. This is likely due to missing variables.")
             print(e)})
  ### WHO-Scale ================================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_who_scale(trial_data, pid, visitid)},
           error = function(e) {
             warning("primary_coding_suep_who_scale() did not work. This is likely due to missing variables.")
             print(e)})
  ### Patient Health Questionnaire =============================================
  tryCatch(expr = {trial_data <- primary_coding_suep_phq4(trial_data)},
           error = function(e) {
             warning("primary_coding_suep_phq4() did not work. This is likely due to missing variables.")
             print(e)})
  ### Pain diagnostic questionnaire ============================================
  tryCatch(expr = {trial_data <- primary_coding_suep_dn4(trial_data)},
           error = function(e) {
             warning("primary_coding_suep_dn4() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## ARDS ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_ards(trial_data)},
           error = function(e) {
             warning("primary_coding_suep_ards() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## PROMIS Recoding ===========================================================
  tryCatch(expr = {trial_data <- primary_coding_suep_recode_promis_dysp(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_recode_promis_dysp() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_recode_promis_cogn_funct(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_recode_promis_cogn_funct() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_recode_promis_sleep(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_recode_promis_sleep() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_recode_promis_phys(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_recode_promis_phys() did not work. This is likely due to missing variables.")
             print(e)})
  tryCatch(expr = {trial_data <- primary_coding_suep_recode_promis_social(trial_data, visitid)},
           error = function(e) {
             warning("primary_coding_suep_recode_promis_social() did not work. This is likely due to missing variables.")
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
#' @param brs.factor BRS item, that needs recoding

recode_brs <- function(brs.factor) {
  
  case_when(brs.factor == "Stimme \u00fcberhaupt nicht zu" ~ 5, 
            brs.factor == "Stimme eher nicht zu" ~ 4,
            brs.factor == "Neutral" ~ 3,
            brs.factor == "Stimme eher zu" ~ 2, 
            brs.factor == "Stimme vollkommen zu" ~ 1,
            brs.factor == "Keine Informationen verf\u00fcgbar" ~ -1)
}


#' Recode Chalder Fatigue Scale (CFQ-11) items
#' 
#' @description recodes the Chalder Fatigue Scale (CFQ-11) items into a dichotomous variable
#' 
#' @param cfq Item of Chalder Fatigue Scale (CFQ-11) that needs to be recoded
#' @return A vector with the correctly coded CFQ-11 item
#' @export

recode_cfq11 <- function(cfq) {
  
  case_when(cfq == -1 ~ -1, 
            cfq <= 2 ~ 0,
            cfq > 2 ~ 1)
  
}


## Chalder Fatigue Scale (CFQ-11) ==========================================

#' Calculate Chalder Fatigue Scale (CFQ-11) sum score
#' 
#' @description Calculate sum score of CFQ-11
#' 
#' items need to be recoded first
#' 
#' @param cfq_1 vector for item "Is fatigue a problem for you?"
#' @param cfq_2 vector for item "Do you need to rest frequently?."
#' @param cfq_3 vector for item "Do you feel tired or sleepy?"
#' @param cfq_4 vector for item "Do you have difficulty getting things done?"
#' @param cfq_5 vector for item "Do you lack energy?"
#' @param cfq_6 vector for item "Do you have less strength in your muscles?"
#' @param cfq_7 vector for item "Do you feel weak?"
#' @param cfq_8 vector for item "Do you find it difficult to concentrate?"
#' @param cfq_9 vector for item "Do you have slips of the tongue when you speak?"
#' @param cfq_10 vector for item "Do you find it difficult to think clearly?"
#' @param cfq_11 vector for item "How is your memory?"
#' 
#' @return A numeric vector with sum score of CFQ-11
#' @export

calculate_cfq11_sum <- function(cfq_1, cfq_2, cfq_3, cfq_4, cfq_5, cfq_6, cfq_7, cfq_8, cfq_9, cfq_10, cfq_11) {
  
  ecu_cfq11_sum <- ifelse(cfq_1 == -1, 0, cfq_1) + 
    ifelse(cfq_2 == -1, 0, cfq_2) +
    ifelse(cfq_3 == -1, 0, cfq_3) + 
    ifelse(cfq_4 == -1, 0, cfq_4) + 
    ifelse(cfq_5 == -1, 0, cfq_5) + 
    ifelse(cfq_6 == -1, 0, cfq_6) + 
    ifelse(cfq_7 == -1, 0, cfq_7) + 
    ifelse(cfq_8 == -1, 0, cfq_8) + 
    ifelse(cfq_9 == -1, 0, cfq_9) +
    ifelse(cfq_10 == -1, 0, cfq_10) + 
    ifelse(cfq_11 == -1, 0, cfq_11)
  
  return(ecu_cfq11_sum)
  
}


#' Categorize Chalder Fatigue Scale (CFQ-11)
#' 
#' @description Categorize CFQ-11
#' 
#' @param ecu_cfq11_sum A numerical vector with CFQ-11 sum score
#' 
#' @return A factor /w levels "No Fatigue" and "Fatigue"
#' @export

categorize_cfq11 <- function(ecu_cfq11_sum) {
  
  ecu_cfq11_cat <- case_when(ecu_cfq11_sum <= 3 ~ "No fatigue",
                             ecu_cfq11_sum >= 4 ~ "Fatigue")
  
  return(ecu_cfq11_cat)
  
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
  
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  visit_data <- trial_data$scv %>%
    full_join (trial_data$m2, by=c(pid, visit_label_var_name, docid)) %>%
    full_join (trial_data$m, by=c(pid, visit_label_var_name, docid)) %>%
    filter(.data$pr_visit_mode.factor != "Zus\u00e4tzliche Dokumentationsvisite" | is.na(.data$pr_visit_mode.factor) |
             .data$pr_visit_mode.factor != "Telefonvisite (PROM)") %>%
    #filter(visit_label_var_name != "3M Follow-Up" | visit_label_var_name != "12M Follow-Up") %>%
    mutate(visit_date = coalesce(.data$gec_pr_docudate_1.date, .data$pr_docudate.date, .data$pr_incl_date.date)) %>%
    arrange(.data$visit_date) %>%
    group_by(!!sym(pid)) %>%
    mutate(!!sym(visit_label_var_name) := str_replace(!!sym(visit_label_var_name), "#", as.character(row_number()))) %>%
    ungroup() %>%
    select(all_of(pid), all_of(visit_label_var_name), "visit_date")
  
  
  # Inclusion groups (one row per pat) -----------------------------------------
  # Definition von paediatrischen Kontrollpatienten (gec_pr_inclusion = 8)
  # Einschlusskriterien fuer paediatrische Kontrollgruppe, wenn eines der folgenden erfuellt ist:
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
      select(all_of(pid),"ecu_pr_inclusion")
  }   else {
    inclusion_data <- trial_data$scv %>%
      mutate(ecu_pr_inclusion = as.character(.data$gec_pr_inclusion.factor)) %>%
      select(all_of(pid),"ecu_pr_inclusion")
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
    select(all_of(pid),"ecu_main_diag_covid")
  
  
  # Date of last Treatment Update ----------------------------------------------
  
  treatment_update_date <- trial_data$fuv3 %>%
    #left_join (trial_data$vp %>% select (.data$mnpvisid, .data$visit_label_var_name), by = "mnpvisid") %>%
    filter (.data$pr_check_treat.factor == "Ja") %>% #only treatment_update == YES
    rename (treatment_update_visit = all_of(visit_label_var_name),
            treatment_update.datetime = "fuv3_date.date") %>%
    group_by (!!sym(pid)) %>%
    slice_max (.data$treatment_update.datetime, with_ties = FALSE) %>%
    ungroup() %>%
    mutate (treatment_update.date = as_date(.data$treatment_update.datetime)) %>%
    select (all_of(pid), "treatment_update_visit", "treatment_update.date") 
  
  
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
      gec_oxy_type.factor = factor(fct_reorder(.data$gec_oxy_type.factor, .data$gec_oxy_type, .na_rm = FALSE), ordered = TRUE)) %>%
    select(all_of(pid),"gec_oxy.factor", "gec_oxy_type", "gec_oxy_type.factor", "gec_oxy_start.date", "gec_oxy_start_d.factor", "gec_oxy_start_uk.factor", 
           "gec_oxy_end.date", "gec_oxy_end_d.factor", "gec_oxy_end_uk.factor", "gec_oxy_end_on.factor", "treatment_update.date", "ecu_oxy_interval") 
  
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
    select(all_of(pid),"ward.factor", starts_with("ecu"), "gec_resid_disch.factor")
  
  # Death (one row per pat) ----------------------------------------------------
  
  eresid_death <- trial_data$eresid %>%
    filter(.data$gec_resid_disch.factor == "Tod") %>%
    rename(ecu_death_date.date = "resid_end.date",
           ecu_death_date_d.factor = "resid_end_d.factor",
           ecu_death_date_uk.factor = "resid_end_uk.factor") %>%
    select(all_of(pid),"ecu_death_date.date", "ecu_death_date_d.factor", "ecu_death_date_uk.factor")
  
  tryCatch(expr = {fv2_6_death <- trial_data$fv2_6 %>%
    filter(.data$gec_death.factor == "Ja" | .data$gec_death.factor == "Keine Informationen verf\u00fcgbar") %>%
    rename(ecu_death_date.date = "death_date.date",
           ecu_death_date_d.factor = "death_date_d.factor",
           ecu_death_date_uk.factor = "death_date_uk.factor") %>%
    select(all_of(pid),"ecu_death_date.date", "ecu_death_date_d.factor", "ecu_death_date_uk.factor")},
    error = function(e) {
      warning("death_date is missing in fv2_6")
      print(e)})
  
  death_data <- fv2_6_death %>%
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
    select(all_of(pid),"ecu_death_date.date", "ecu_death_date_d.factor", "ecu_death_date_uk.factor")
  
  
  # WHO Scale Data  (one row per pat per visit) --------------------------------
  
  who_scale_per_visit_data <- visit_data %>%
    full_join(trial_data$fv15 %>% select(all_of(pid), "pr_end_date.date", "pr_end_reason.factor"), by=pid) %>%
    full_join(inclusion_data, by =pid) %>%
    full_join(main_diag_data, by =pid) %>%
    full_join(death_data, by=pid) %>%
    full_join(oxy_data, by=pid, relationship = "many-to-many") %>%
    full_join(hospital_data, by=pid, relationship = "many-to-many") %>%
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
      ecu_who_scale_with_diag.factor = case_when(.data$ecu_who_scale.factor == "Hospitalisiert, moderate Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Covid" ~ "Hospitalisiert wegen Covid, moderate Phase",
                                                 .data$ecu_who_scale.factor == "Hospitalisiert, moderate Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Andere" ~ "Hospitalisiert mit Covid, moderate Phase",
                                                 .data$ecu_who_scale.factor == "Hospitalisiert, schwere Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Covid" ~ "Hospitalisiert wegen Covid, schwere Phase",
                                                 .data$ecu_who_scale.factor == "Hospitalisiert, schwere Phase" & .data$ecu_main_diag_covid == "Hauptdiagnose Andere" ~ "Hospitalisiert mit Covid, schwere Phase",
                                                 is.na(.data$ecu_main_diag_covid) ~ NA,
                                                 .data$ecu_main_diag_covid == "Keine Informationen verf\u00fcgbar" ~ "Keine Informationen verf\u00fcgbar",
                                                 TRUE ~ .data$ecu_who_scale.factor),
      ecu_who_scale_with_diag = as.integer(case_when(.data$ecu_who_scale_with_diag.factor == "Keine Informationen verf\u00fcgbar" ~ -1,
                                                     .data$ecu_who_scale_with_diag.factor == "Kontrollgruppe, ohne Sars-Infektion" ~ 0,
                                                     .data$ecu_who_scale_with_diag.factor == "Ambulant, milde Phase" ~ 1,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert mit Covid, moderate Phase" ~ 2,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert wegen Covid, moderate Phase" ~ 3,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert mit Covid, schwere Phase" ~ 4,
                                                     .data$ecu_who_scale_with_diag.factor == "Hospitalisiert wegen Covid, schwere Phase" ~ 5,
                                                     .data$ecu_who_scale_with_diag.factor == "Verstorben" ~ 6))
    ) 
  
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
    slice_max(.data$ecu_who_scale, with_ties = FALSE) %>% 
    ungroup() %>%
    rename(ecu_who_scale_max = "ecu_who_scale",
           ecu_who_scale_max.factor = "ecu_who_scale.factor") %>%
    select(all_of(pid), "ecu_who_scale_max", "ecu_who_scale_max.factor")
  
  who_scale_with_diag_max <- trial_data[["ecu_who_scale_per_visit_data"]] %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$ecu_who_scale_with_diag, with_ties = FALSE) %>%
    ungroup() %>%
    rename(ecu_who_scale_with_diag_max = "ecu_who_scale_with_diag",
           ecu_who_scale_with_diag_max.factor = "ecu_who_scale_with_diag.factor") %>%
    select(all_of(pid), "ecu_who_scale_with_diag_max", "ecu_who_scale_with_diag_max.factor")
  
  trial_data[["scv"]] <- trial_data[["scv"]] %>%
    left_join(who_scale_max, by = pid) %>%
    left_join(who_scale_with_diag_max, by = pid)
  
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
#' @return A dataframe with one row per symptom per patient
#' @export

build_suep_long_symptom_df <- function(trial_data, pid){
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  # Date of Screening visit
  if("gec_pr_incl_date.date" %in% names(trial_data$scv)) {scv_date <- trial_data$scv %>%
    select("gec_pr_incl_date.date", all_of(pid))} else {scv_date <- trial_data$scv %>%
      select("pr_incl_date.date", all_of(pid))}
  
  # Date of Study visits
  visit_date <- trial_data$m2 %>%
    #left_join(select(trial_data$vp, .data$mnpvisid, visit_label_var_name), by = c("mnpvisid", "visit_label_var_name")) %>%
    select(all_of(pid), all_of(visit_label_var_name), "gec_pr_docudate_1.date") %>%
    mutate(visit_label_var_name = case_when(!!sym(visit_label_var_name) == "Abschluss des Akutverlaufs" ~ "end_acute.date", 
                                            !!sym(visit_label_var_name) == "3M Follow-Up" ~ "m3_fup.date", 
                                            !!sym(visit_label_var_name) == "12M Follow-Up" ~ "m12_fup.date")) %>%
    pivot_wider(names_from = visit_label_var_name, values_from = "gec_pr_docudate_1.date", id_cols = all_of(pid))
  
  visit_date_long <- trial_data$m2 %>%
    #left_join(select(trial_data$vp, .data$mnpvisid, visit_label_var_name), by = c("mnpvisid", "visit_label_var_name")) %>%
    rename(visit_label = all_of(visit_label_var_name),
           visit_date.date = "gec_pr_docudate_1.date") %>%
    select(all_of(pid),"visit_label", "visit_date.date")
  
  # Date of last Symptom Update
  symptom_update_date <- trial_data$fuv3 %>%
    #left_join(select(trial_data$vp, .data$mnpvisid, visit_label_var_name), by = c("mnpvisid", "visit_label_var_name")) %>%
    rename(symptom_update_visit = all_of(visit_label_var_name), 
           symptom_update.datetime = "fuv3_date.date") %>%
    group_by(!!sym(pid)) %>%
    slice_max(.data$symptom_update.datetime, with_ties = FALSE) %>% # keep row for each pid which contains max(symptom_update.datetime) 
    ungroup() %>%
    select(all_of(pid),"symptom_update_visit", "symptom_update.datetime")
  
  
  # Long symptom data with update symptom times
  long_symptom_data <- build_sy_parents_children_merged_df(trial_data, pid) %>%
    left_join(scv_date, by = pid) %>%
    left_join(visit_date_long, by = pid, relationship = "many-to-many") %>%
    filter(.data$visit_label != "Abschluss des Akutverlaufs") %>%
    left_join(visit_date, by = pid) %>%
    left_join(symptom_update_date, by = pid) %>%
    mutate(symptom_update_visit.date = case_when(.data$symptom_update_visit == "Abschluss des Akutverlaufs" ~ end_acute.date, 
                                                 .data$symptom_update_visit == "3M Follow-Up" ~ m3_fup.date, 
                                                 .data$symptom_update_visit == "12M Follow-Up" ~ m12_fup.date)) %>%
    select(-c("end_acute.date", "m3_fup.date", "m12_fup.date")) %>%
    mutate(sy_end_min.date = case_when(.data$sy_dur.factor == "Andauernd" ~ symptom_update_visit.date, 
                                       .data$sy_dur.factor == "181-365 Tage (6-12 Monate)"~ sy_start.date + 181, 
                                       .data$sy_dur.factor == "181 - 365 Tage (6-12 Monate)"~ sy_start.date + 181,
                                       .data$sy_dur.factor == "121-180 Tage (4-6 Monate)"~ sy_start.date + 121, 
                                       .data$sy_dur.factor == "61-120 Tage (2-4 Monate)"~ sy_start.date + 61, 
                                       .data$sy_dur.factor == "31-60 Tage (1-2 Monate)"~ sy_start.date + 31, 
                                       .data$sy_dur.factor == "15-30 Tage (2-4 Wochen)"~ sy_start.date + 15,
                                       .data$sy_dur.factor == "8-14 Tage (1-2 Wochen)"~ sy_start.date + 8,
                                       .data$sy_dur.factor == "7 Tage"~ sy_start.date + 7,
                                       .data$sy_dur.factor == "6 Tage"~ sy_start.date + 6,
                                       .data$sy_dur.factor == "5 Tage"~ sy_start.date + 5,
                                       .data$sy_dur.factor == "4 Tage"~ sy_start.date + 4,
                                       .data$sy_dur.factor == "3 Tage"~ sy_start.date + 3,
                                       .data$sy_dur.factor == "2 Tage"~ sy_start.date + 2,
                                       .data$sy_dur.factor == "1 Tag"~ sy_start.date + 1), 
           sy_time_to_visit = .data$visit_date.date - .data$sy_start.date,
           sy_dur_min = .data$sy_end_min.date - .data$sy_start.date,
           sy_end_max.date = case_when(.data$sy_dur.factor == "Andauernd" ~ symptom_update_visit.date, 
                                       .data$sy_dur.factor == "181-365 Tage (6-12 Monate)"~ sy_start.date + days(365), 
                                       .data$sy_dur.factor == "181 - 365 Tage (6-12 Monate)"~ sy_start.date + days(365),
                                       .data$sy_dur.factor == "121-180 Tage (4-6 Monate)"~ sy_start.date + days(180), 
                                       .data$sy_dur.factor == "61-120 Tage (2-4 Monate)"~ sy_start.date + days(120), 
                                       .data$sy_dur.factor == "31-60 Tage (1-2 Monate)"~ sy_start.date + days(60), 
                                       .data$sy_dur.factor == "15-30 Tage (2-4 Wochen)"~ sy_start.date + days(30),
                                       .data$sy_dur.factor == "8-14 Tage (1-2 Wochen)"~ sy_start.date + days(14),
                                       .data$sy_dur.factor == "7 Tage"~ sy_start.date + days(7),
                                       .data$sy_dur.factor == "6 Tage"~ sy_start.date + days(6),
                                       .data$sy_dur.factor == "5 Tage"~ sy_start.date + days(5),
                                       .data$sy_dur.factor == "4 Tage"~ sy_start.date + days(4),
                                       .data$sy_dur.factor == "3 Tage"~ sy_start.date + days(3),
                                       .data$sy_dur.factor == "2 Tage"~ sy_start.date + days(2),
                                       .data$sy_dur.factor == "1 Tag"~ sy_start.date + days(1)), 
           sy_dur_max =  .data$sy_end_max.date - .data$sy_start.date,
           sy_end_applicable = case_when(((.data$sy_d.factor == "Auf Jahr genau" |  .data$sy_d.factor == "Unbekannter Zeitpunkt vor Baseline") & .data$sy_dur.factor != "Andauernd") ~ FALSE, #"Auf Jahr genau" und "Unbekannter Zeitpunkt vor Baseline" kann nur mit "Andauernd" ausgewertet werden. 
                                         .data$sy_dur.factor == "Keine Informationen verf\u00fcgbar" ~ FALSE,
                                         .data$sy_dur.factor == "Andauernd" & is.na(.data$symptom_update_visit.date) ~ FALSE, 
                                         .data$sy_start.date < 2020-01-01 ~ FALSE, 
                                         (((.data$sy_d.factor == "Auf Monat genau" | #Addition von sy_dur funktioniert nur bei "Auf Monat genau" oder genauerem Symptomstart
                                              .data$sy_d.factor == "Auf Woche genau" | .data$sy_d.factor == "Exakte Angabe") & .data$sy_dur.factor != "Keine Informationen verf\u00fcgbar") | 
                                            (.data$sy_dur.factor == "Andauernd" & !is.na(.data$symptom_update_visit.date)) |
                                            .data$sy_parent == "Nein") & #wenn das Symptom nicht vorliegt, ist ein fehlendes Datum in Ordnung
                                           .data$sy_start.date >= 2020-01-01 #wenn das Symptom vor 2020 begonnen hat, kann es nicht mit Covid in Verbindung stehen
                                         ~ TRUE), 
           sy_end_applicable.factor = as.factor(case_when(.data$sy_end_applicable ~ "Symptomdauer kann ausgewertet werden.", 
                                                          !.data$sy_end_applicable ~ "Symptomdauer kann NICHT ausgewertet werden.")))
  
  return(long_symptom_data)
}


## Post Covid Syndrome Score (PCSS) =============================================

#' Calculate symptom complex for Post Covid Syndrome Score (PCSS) calculation
#' 
#' @param symptom_names A vector with names of needed symptoms
#' @param vector_of_pcss_fup_visits A vector with labels of desired visit
#' @param days_of_pcss_time_diff A vector with number of days of minimum symptom duration
#' @param sy_time_to_visit A vector with number of days between symptom start date and visit date
#' @param sy_extracted_name_parent A vector with parent form table name for needed symptoms
#' @param sy_parent A vector for symptom parent question /w levels "Ja" and "Nein" indicating whether the parent question has been answered with
#' "yes" or "no"
#' @param symptom_update_visit A vector indication the visit at which symptoms were updated last
#' @param symptom_update_visit.date A vector indication the date at which symptoms were updated last
#' @param sy_end_applicable A vector indicating whether the end date of symptom duration is applicable for time span calculation
#' @param visit_date.date A vector with visit date
#' @param sy_end_min.date A vector with calculated minimum date of symptom end
#' @param sy_end_max.date A vector with calculated maximum date of symptom end
#' @param sy_dur_min A vector indicating the minimum number of days of symptom duration
#' @param sy_dur_max A vector indicating the maximum number of days of symptom duration

detect_symptom_complex <- function(symptom_names, 
                                   vector_of_pcss_fup_visits, 
                                   days_of_pcss_time_diff,
                                   sy_time_to_visit,
                                   sy_extracted_name_parent, 
                                   sy_parent, 
                                   symptom_update_visit, 
                                   symptom_update_visit.date, 
                                   sy_end_applicable, 
                                   visit_date.date,
                                   sy_end_min.date, sy_end_max.date,
                                   sy_dur_min, sy_dur_max) {
  case_when(
    
    # wenn 3M oder 12M nicht stattgefunden hat, koennen keine Symptomkomplexe festgestellt oder ausgeschlossen werden
    !symptom_update_visit %in% vector_of_pcss_fup_visits ~ NA_real_, 
    
    # wenn sy_parent == "Keine Informationen verfuegbar" oder sy_end_applicable = FALSE, kann dieses Symptom nicht festgestellt oder ausgeschlossen werden
    sy_extracted_name_parent %in% symptom_names & (sy_parent == "Keine Informationen verf\u00fcgbar" | !sy_end_applicable) ~ NA_real_, 
    
    # wenn das Symptom mindestens nach in days_to_pcss_time_diff geforderten Tagen und zur Visite vorliegt, ist der Symptomkomplex zutreffend
    sy_extracted_name_parent %in% symptom_names &  sy_parent == "Ja" & days_of_pcss_time_diff <= 180 & sy_time_to_visit >= days_of_pcss_time_diff & sy_end_min.date >= visit_date.date & sy_end_applicable ~ 1, 
    sy_extracted_name_parent %in% symptom_names &  sy_parent == "Ja" & days_of_pcss_time_diff > 180 & sy_time_to_visit >= days_of_pcss_time_diff & sy_end_max.date >= visit_date.date & sy_end_applicable ~ 1, 
    
    # wenn das Symptom nicht vorliegt, ist der Symptomkomplex nicht zutreffend
    sy_extracted_name_parent %in% symptom_names &  sy_parent == "Nein" ~ 0,
    
    # wenn das Symptom weniger als (maximal) in days_to_pcss_time_diff geforderten Tagen vorliegt, ist der Symptomkomplex nicht zutreffend
    sy_extracted_name_parent %in% symptom_names &  sy_parent == "Ja" &  sy_time_to_visit < days_of_pcss_time_diff & sy_end_applicable ~ 0,
    
    # wenn das Symptom die geforderte Anzahl an Tagen , aber bei der gewuenschten Visite nicht mehr vorliegt, ist der Symptomkomplex nicht zutreffend
    sy_extracted_name_parent %in% symptom_names &  sy_parent == "Ja" &  sy_time_to_visit >= days_of_pcss_time_diff & sy_end_max.date < visit_date.date & sy_end_applicable ~ 0,
    
    # wenn aufgrund der unpraezisen Symptomdauer nicht festgestellt werden kann, ob das Symptom nach der in days_of_pcss_time_diff geforderten Anzahl an Tagen vorliegt, 
    # kann dieses Symptom nicht festgestellt oder ausgeschlossen werden
    sy_extracted_name_parent %in% symptom_names &  sy_parent == "Ja" &  sy_dur_max >= days_of_pcss_time_diff & sy_dur_min < days_of_pcss_time_diff ~ NA_real_
  )
}


#' Calculate symptom complex for Post Covid Syndrome Score (PCSS) calculation - for symptoms coded in "other"
#' 
#' @param symptom_names A vector with names of needed symptoms
#' @param vector_of_pcss_fup_visits A vector with labels of desired visit
#' @param days_of_pcss_time_diff A vector with number of days of minimum symptom duration
#' @param sy_time_to_visit A vector with number of days between symptom start date and visit date
#' @param sy_oth_icd11name A vector with names of symptoms documented in "Other Symptoms"
#' @param sy_parent A vector for symptom parent question /w levels "Ja" and "Nein" indicating whether the parent question has been answered with
#' "yes" or "no"
#' @param symptom_update_visit A vector indication the visit at which symptoms were updated last
#' @param symptom_update_visit.date A vector indication the date at which symptoms were updated last
#' @param sy_end_applicable A vector indicating whether the end date of symptom duration is applicable for time span calculation
#' @param visit_date.date A vector with visit date
#' @param sy_end_min.date A vector with calculated minimum date of symptom end
#' @param sy_end_max.date A vector with calculated maximum date of symptom end
#' @param sy_dur_min A vector indicating the minimum number of days of symptom duration
#' @param sy_dur_max A vector indicating the maximum number of days of symptom duration

detect_other_symptom_of_complex <- function(symptom_names, 
                                            vector_of_pcss_fup_visits, 
                                            days_of_pcss_time_diff,
                                            sy_time_to_visit,
                                            sy_oth_icd11name, 
                                            sy_parent, 
                                            symptom_update_visit, 
                                            symptom_update_visit.date, 
                                            sy_end_applicable, 
                                            visit_date.date,
                                            sy_end_min.date, sy_end_max.date,
                                            sy_dur_min, sy_dur_max) {
  # es kann dokumentiert werden, dass "other symptoms" vorliegen: sy_extracted_name_parent == "other" & sy_parent == "Ja"
  # im Anschluss kann unter sy_oth_icd11name das Symptom benannt werden. 
  # da dieses Symptom niemals als "nicht vorhanden" dokumentiert wird, wird es immer ausgeschlossen, wenn es nicht dokumentiert ist. 
  
  case_when(
    
    # wenn 3M oder 12M nicht stattgefunden hat oder sy_end_applicable = FALSE, kann dieses Symptom nach 3M nicht festgestellt oder ausgeschlossen werden
    !symptom_update_visit %in% vector_of_pcss_fup_visits ~ NA_real_,
    
    sy_oth_icd11name %in% symptom_names & !sy_end_applicable ~ NA_real_, 
    
    # wenn das Symptom mindestens nach in days_to_pcss_time_diff geforderten Tagen sowie zur Visite vorliegt, ist der Symptomkomplex zutreffend
    sy_oth_icd11name %in% symptom_names &  sy_parent == "Ja" & days_of_pcss_time_diff <= 181 & sy_time_to_visit >= days_of_pcss_time_diff & sy_end_min.date >= visit_date.date & sy_end_applicable ~ 1, 
    sy_oth_icd11name %in% symptom_names &  sy_parent == "Ja" & days_of_pcss_time_diff > 181 & sy_time_to_visit >= days_of_pcss_time_diff & sy_end_max.date >= visit_date.date & sy_end_applicable ~ 1, 
    
    # wenn das Symptom weniger als (maximal) in days_to_pcss_time_diff geforderten Tagen vorliegt, ist der Symptomkomplex nicht zutreffend
    sy_oth_icd11name %in% symptom_names &  sy_parent == "Ja" &  sy_time_to_visit < days_of_pcss_time_diff &  sy_end_applicable ~ 0,
    
    # wenn das Symptom die geforderte Anzahl an Tagen , aber bei der gewuenschten Visite nicht mehr vorliegt, ist der Symptomkomplex nicht zutreffend
    sy_oth_icd11name %in% symptom_names &  sy_parent == "Ja" &  sy_time_to_visit >= days_of_pcss_time_diff & sy_end_max.date < visit_date.date & sy_end_applicable ~ 0,
    
    # wenn aufgrund der unpraezisen Symptomdauer nicht festgestellt werden kann, ob das Symptom nach in days_to_pcss_time_diff geforderten Tagen vorliegt, kann dieses Symptom nicht festgestellt oder ausgeschlossen werden
    sy_oth_icd11name %in% symptom_names &  sy_parent == "Ja" &  sy_dur_max >= days_of_pcss_time_diff & sy_dur_min < days_of_pcss_time_diff ~ NA_real_
  )
}


#' Calculate PCS Score
#' 
#' for each symptom-row in long data, apply corresponding the detect_symptom_complex and summarise long symptom data frame by PCSS symptom complex
#' returns a dataframe with one row per pid containing the results of PCS Score 
#' 
#' @param trial_data SecuTrial Data
#' @param pid column name of patient ID in data
#' @param days_of_pcss_time_diff Time difference since COVID-19 diagnosis, for which PCSS should be calculated, in days
#' @param vector_of_pcss_fup_visits Vector of Follow-Up Visits that are sufficient to evaluate PCSS, based on PCSS time difference
#' @return A dataframe with symptom complexes and PCSS-score for specified Follow-Up visits
#' @importFrom rlang .data
#' @export

calculate_pcs_score_suep <- function(trial_data, pid, days_of_pcss_time_diff, vector_of_pcss_fup_visits) {
  
  long_symptom_data_pcss <- trial_data$ecu_long_symptom_data %>%
    mutate(complex_1_chemo = detect_symptom_complex(symptom_names = c("gec_sy_ne_taste", "gec_sy_ne_smell"), 
                                                    vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                    .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                    .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max), 
           complex_2_fatigue_oth = detect_other_symptom_of_complex(symptom_names = c("Fatigue"),
                                                                   vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_oth_icd11name, 
                                                                   .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, 
                                                                   .data$visit_date.date, .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_3_exercise = detect_symptom_complex(symptom_names = c("gec_sy_pd_dysp"), 
                                                       vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                       .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                       .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_4_pain = detect_symptom_complex(symptom_names = c("sy_arthr", "sy_myalg"), 
                                                   vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                   .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                   .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_5_ent = detect_symptom_complex(symptom_names = c("sy_pd_rhin", "sy_pd_stuf", "sy_pd_sneez", "sy_pd_sore"), 
                                                  vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                  .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                  .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_6_cough = detect_symptom_complex(symptom_names = c("gec_sy_pd_cough", "sy_pd_wheez"), 
                                                    vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                    .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                    .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_7_chest = detect_symptom_complex(symptom_names = c("sy_breastp"), 
                                                    vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                    .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                    .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_8_gastro = detect_symptom_complex(symptom_names = c("gec_sy_gi_abdp", "gec_sy_gi_diar", "gec_sy_gi_vom", "gec_sy_gi_naus"), 
                                                     vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                     .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                     .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           # Sonderfall Neuro: die einzelnen symptome stammen aus unterschiedlichen formularen. um den symptomkomplex auszuschliessen, muessen wir jeweils die uebergeordnete Frage gec_sy bzw. sy_ne und die jeweils untergeordneten Einzelsymptome auswerten.
           complex_9_neuro_gec = detect_symptom_complex(symptom_names = c("gec_sy_ne_conf", "sy_ne_cogn"), 
                                                        vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                        .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                        .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_9_neuro_ne = detect_symptom_complex(symptom_names = c("sy_dizzi", "gec_sy_heada"), 
                                                       vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                       .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                       .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_10_derma = detect_symptom_complex(symptom_names = c("sy_skin"), 
                                                     vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                     .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                     .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_11_flulike_sy = detect_symptom_complex(symptom_names = c("gec_sy_fever", "sy_appet", "sy_lymph"), 
                                                          vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_extracted_name_parent, 
                                                          .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                          .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_11_flulike_oth = detect_other_symptom_of_complex(symptom_names = c("Chills", "Feeling ill"),
                                                                    vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_oth_icd11name, 
                                                                    .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                                    .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max),
           complex_12_sleep_oth = detect_other_symptom_of_complex(symptom_names = c("Sleep disturbance, not elsewhere classified"),
                                                                  vector_of_pcss_fup_visits, days_of_pcss_time_diff, .data$sy_time_to_visit, .data$sy_oth_icd11name, 
                                                                  .data$sy_parent, .data$symptom_update_visit, .data$symptom_update_visit.date, .data$sy_end_applicable, .data$visit_date.date, 
                                                                  .data$sy_end_min.date, .data$sy_end_max.date, .data$sy_dur_min, .data$sy_dur_max)
    )
  
  symptom_complex_suep_summary <- long_symptom_data_pcss %>%
    group_by(!!sym(pid), .data$visit_label) %>%
    summarise(complex_1_chemo_sum = case_when(any(.data$sy_extracted_name_parent == "sy_ne" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0, # wenn bei der uebergeordeten Symptomfrage "Nein" geantwortet wurde, schliessen wir den Sy-Komplex aus.
                                              any(.data$complex_1_chemo == 1) ~ 1, 
                                              any(.data$complex_1_chemo == NA_real_) ~ NA_real_,
                                              any(.data$complex_1_chemo == 0) ~ 0), # wenn kein 1 oder NA_real_ dokumentiert ist, und mindestens eine 0, schliessen wir den Sy-Komplex aus.
              complex_2_fatigue_sum = case_when(all(.data$sy_oth_icd11name != "Fatigue" | is.na(.data$sy_oth_icd11name)) & any(.data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,# wenn die Symptome aktuell sind und niemals Fatigue dokumentiert wurde, wird der Symptomkomplex ausgeschlossen
                                                any(.data$complex_2_fatigue_oth == 1) ~ 1,
                                                any(.data$complex_2_fatigue_oth == NA_real_) ~ NA_real_,
                                                any(.data$complex_2_fatigue_oth == 0) ~ 0),
              complex_3_exercise_sum = case_when(any(.data$sy_extracted_name_parent == "sy_pd" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                                 any(.data$complex_3_exercise == 1) ~ 1, 
                                                 any(.data$complex_3_exercise == NA_real_) ~ NA_real_, 
                                                 any(.data$complex_3_exercise == 0) ~ 0), 
              complex_4_pain_sum = case_when(any(.data$sy_extracted_name_parent == "gec_sy" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                             any(.data$complex_4_pain == 1) ~ 1, 
                                             any(.data$complex_4_pain == NA_real_) ~ NA_real_,
                                             any(.data$complex_4_pain == 0) ~ 0), 
              complex_5_ent_sum = case_when(any(.data$sy_extracted_name_parent == "sy_pd" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                            any(.data$complex_5_ent == 1) ~ 1, 
                                            any(.data$complex_5_ent == NA_real_) ~ NA_real_, 
                                            any(.data$complex_5_ent == 0) ~ 0), 
              complex_6_cough_sum = case_when(any(.data$sy_extracted_name_parent == "sy_pd" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                              any(.data$complex_6_cough == 1) ~ 1, 
                                              any(.data$complex_6_cough == NA_real_) ~ NA_real_, 
                                              any(.data$complex_6_cough == 0) ~ 0), 
              complex_7_chest_sum = case_when(any(.data$sy_extracted_name_parent == "gec_sy" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                              any(.data$complex_7_chest == 1) ~ 1, 
                                              any(.data$complex_7_chest == NA_real_) ~ NA_real_,  
                                              any(.data$complex_7_chest == 0) ~ 0), 
              complex_8_gastro_sum = case_when(any(.data$sy_extracted_name_parent == "sy_gi" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                               any(.data$complex_8_gastro == 1) ~ 1,  
                                               any(.data$complex_8_gastro == NA_real_) ~ NA_real_,  
                                               any(.data$complex_8_gastro == 0) ~ 0),
              # Sonderfall Neuro: die einzelnen symptome stammen aus unterschiedlichen formularen. um den symptomkomplex auszuschliessen, muessen wir jeweils die uebergeordnete Frage gec_sy bzw. sy_ne und die jeweils untergeordneten Einzelsymptome auswerten.
              complex_9_neuro_gec_sum = case_when(any(.data$sy_extracted_name_parent == "gec_sy" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                                  any(.data$complex_9_neuro_gec == 1) ~ 1,  
                                                  any(.data$complex_9_neuro_gec == NA_real_) ~ NA_real_,  
                                                  any(.data$complex_9_neuro_gec == 0) ~ 0), 
              complex_9_neuro_ne_sum = case_when(any(.data$sy_extracted_name_parent == "sy_ne" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                                 any(.data$complex_9_neuro_ne == 1) ~ 1,  
                                                 any(.data$complex_9_neuro_ne == NA_real_) ~ NA_real_,  
                                                 any(.data$complex_9_neuro_ne == 0) ~ 0), 
              complex_10_derma_sum = case_when(any(.data$sy_extracted_name_parent == "gec_sy" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                               any(.data$complex_10_derma == 1) ~ 1,  
                                               any(.data$complex_10_derma == NA_real_) ~ NA_real_,  
                                               any(.data$complex_10_derma == 0) ~ 0), 
              # Sonderfall Flulike Symptoms: die einzelnen symptome koennen im Formula "allgemeine Symptome"(gec_sy) und unter "andere Symptome" dokumentiert werden. um den symptomkomplex auszuschliessen, muessen wir jeweils die uebergeordnete Frage gec_sy und die jeweils untergeordneten Einzelsymptome/andere Symptome auswerten.
              complex_11_flulike_sy_sum = case_when(any(.data$sy_extracted_name_parent == "gec_sy" &  .data$sy_parent == "Nein" &  .data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,
                                                    any(.data$complex_11_flulike_sy == 1) ~ 1,  
                                                    any(.data$complex_11_flulike_sy == NA_real_) ~ NA_real_,  
                                                    any(.data$complex_11_flulike_sy == 0) ~ 0), 
              complex_11_flulike_oth_sum = case_when(all((.data$sy_oth_icd11name != "Chills" &  .data$sy_oth_icd11name != "Feeling ill") | is.na(.data$sy_oth_icd11name)) & any(.data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,# wenn die Symptome aktuell sind und niemals Fatigue dokumentiert wurde, wird der Symptomkomplex ausgeschlossen
                                                     any(.data$complex_11_flulike_oth == 1) ~ 1,
                                                     any(.data$complex_11_flulike_oth == NA_real_) ~ NA_real_,
                                                     any(.data$complex_11_flulike_oth == 0) ~ 0), 
              complex_12_sleep_sum = case_when(all(.data$sy_oth_icd11name != "Sleep disturbance, not elsewhere classified"| is.na(.data$sy_oth_icd11name)) & any(.data$symptom_update_visit %in% vector_of_pcss_fup_visits) ~ 0,# wenn die Symptome aktuell sind und niemals Fatigue dokumentiert wurde, wird der Symptomkomplex ausgeschlossen
                                               any(.data$complex_12_sleep_oth == 1) ~ 1,
                                               any(.data$complex_12_sleep_oth == NA_real_) ~ NA_real_,
                                               any(.data$complex_12_sleep_oth == 0) ~ 0)
    ) %>%
    ungroup() %>%
    mutate(complex_9_neuro_sum = case_when(.data$complex_9_neuro_gec_sum == 1 |  .data$complex_9_neuro_ne_sum == 1 ~ 1,
                                           .data$complex_9_neuro_gec_sum == NA_real_ |  .data$complex_9_neuro_ne_sum == NA_real_ ~ NA_real_,
                                           .data$complex_9_neuro_gec_sum == 0 &  .data$complex_9_neuro_ne_sum == 0 ~ 0), 
           complex_11_flulike_sum = case_when(.data$complex_11_flulike_sy_sum == 1 |  .data$complex_11_flulike_oth_sum == 1 ~ 1,
                                              .data$complex_11_flulike_sy_sum == NA_real_ |  .data$complex_11_flulike_oth_sum == NA_real_ ~ NA_real_,
                                              .data$complex_11_flulike_sy_sum == 0 &  .data$complex_11_flulike_oth_sum == 0 ~ 0)
    )
  
  pcs_score_result_df <- symptom_complex_suep_summary %>% 
    filter(.data$visit_label %in% vector_of_pcss_fup_visits) %>%
    mutate(pcs_score_sum_without_proms = if_else(.data$complex_1_chemo_sum == 1, 3.5, 0) + 
             if_else(.data$complex_2_fatigue_sum == 1, 7.0, 0) +
             if_else(.data$complex_3_exercise_sum == 1, 4.0, 0) +
             if_else(.data$complex_4_pain_sum == 1, 6.5, 0) +
             if_else(.data$complex_5_ent_sum == 1, 5.5, 0) +
             if_else(.data$complex_6_cough_sum == 1, 7.0, 0) +
             if_else(.data$complex_7_chest_sum == 1, 3.5, 0) +
             if_else(.data$complex_8_gastro_sum == 1, 5.0, 0) +
             if_else(.data$complex_9_neuro_sum == 1, 6.5, 0) +
             if_else(.data$complex_10_derma_sum == 1, 2.0, 0) +
             if_else(.data$complex_11_flulike_sum == 1, 3.5, 0) +
             if_else(.data$complex_12_sleep_sum == 1, 5.0, 0), 
           pcs_score_group_without_proms = cut(.data$pcs_score_sum_without_proms,
                                               breaks= c(-Inf, 0, 10.75, 26.25, Inf ), 
                                               labels=c("0", "<=10,75", "10,75<x<=26,25", ">26,25")))
  
  return(pcs_score_result_df)
}


#' Create data frame with Symptom Complexes and Post-COVID variables without PROMs in Post-COVID-Score
#' 
#' returns a dataframe, which contains the symptom complexes and Post-COVID variables without PROMs in Post-COVID-Score
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in data
#' @return A dataframe with symptom compelexes and Post-COVID variables without PROMs in Post-COVID-Score
#' @importFrom rlang .data
#' @export

build_pcs_score_suep_df_without_proms <- function(trial_data, pid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  ecu_pcs_score_3m <- calculate_pcs_score_suep(trial_data, pid, days_of_pcss_time_diff = 61, vector_of_pcss_fup_visits = c("3M Follow-Up", "12M Follow-Up")) 
  ecu_pcs_score_12m <- calculate_pcs_score_suep(trial_data, pid, days_of_pcss_time_diff = 335, vector_of_pcss_fup_visits = c("12M Follow-Up")) 
  
  ecu_pcs_score_3m <- ecu_pcs_score_3m %>%
    filter(.data$visit_label == "3M Follow-Up") 
  
  ecu_pcs_score <- ecu_pcs_score_3m %>%
    full_join(ecu_pcs_score_12m) %>%
    select(all_of(pid),"visit_label", contains("pcs_score"))
  
  return(ecu_pcs_score)
  
}

#' Create data frame with Symptom Complexes and Post-COVID variables with and without PROMs in Post-COVID-Score
#' 
#' returns a dataframe, which contains the symptom complexes and Post-COVID variables with and without PROMs in Post-COVID-Score
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in data
#' @return A dataframe with symptom compelexes and Post-COVID variables with and without PROMs in Post-COVID-Score
#' @importFrom rlang .data
#' @export

build_pcs_score_suep_df_with_proms <- function(trial_data, pid) {
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid 
  visitid <- trial_data$export_options$id_names$visitid
  docid <- trial_data$export_options$id_names$docid
  visit_label_var_name <- ifelse("mnpvislabel" %in% names(trial_data$m2), "mnpvislabel", "visit_name")
  
  ecu_pcs_score_3m <- calculate_pcs_score_suep(trial_data, pid, days_of_pcss_time_diff = 61, vector_of_pcss_fup_visits = c("3M Follow-Up", "12M Follow-Up")) 
  ecu_pcs_score_12m <- calculate_pcs_score_suep(trial_data, pid, days_of_pcss_time_diff = 335, vector_of_pcss_fup_visits = c("12M Follow-Up")) 
  
  if (!("ecu_cfq11_sum" %in% names(trial_data$promext))) {trial_data <- primary_coding_suep_cfq11(trial_data, visitid)}
  trial_data <- primary_coding_suep_promis_29_fatigue(trial_data, visitid)
  trial_data <- primary_coding_suep_promis_29_dyspnea(trial_data, visitid)
  trial_data <- primary_coding_suep_promis_cogn_funct(trial_data, visitid)
  trial_data <- primary_coding_suep_promis_29_sleep(trial_data, visitid)
  
  prom <- trial_data$prom %>%
    filter(!!sym(visit_label_var_name) == "3M Follow-Up" | !!sym(visit_label_var_name) == "12M Follow-Up")
  promext <- trial_data$promext %>%
    filter(!!sym(visit_label_var_name) == "3M Follow-Up" | !!sym(visit_label_var_name) == "12M Follow-Up")
  
  relevante_proms <- prom %>%
    left_join(promext, by = c(pid, visit_label_var_name)) %>%
    select(all_of(pid), all_of(visit_label_var_name), "cfs.factor", "ecu_cfq11_sum", "ecu_cfq11_cat", "cfs_seid_crit2.factor", "cfs_seid_crit4.factor", 
           "cfs_seid_crit5.factor", "ecu_promis29_fatigue_sum", "ecu_promis29_fatigue_cat_2", "dysp.factor", "ecu_promis29_dyspnea_n", 
           "ecu_promis29_dyspnea_sum", "ecu_promis29_dyspnea_cat_2", "pain_loc_chest.factor", "pain_loc_abd.factor", "pain_loc_head.factor", 
           "pain_dn2_6.factor", "ecu_promis_cogn_funct_sum", "ecu_promis_cogn_funct_cat_2", "ecu_promis29_sleep_sum", "ecu_promis29_sleep_cat", 
           "ecu_promis29_sleep_cat_2")
  
  ecu_pcs_score_3m <- ecu_pcs_score_3m %>%
    left_join(relevante_proms, by = c(pid, "visit_label" = visit_label_var_name)) %>%
    mutate(complex_2_fatigue_sum_screen = case_when(.data$complex_2_fatigue_sum == 1 | .data$cfs.factor == "Ja" ~ 1,
                                                    TRUE ~ .data$complex_2_fatigue_sum),
           complex_2_fatigue_sum_promis29 = case_when(.data$complex_2_fatigue_sum == 1 | .data$ecu_promis29_fatigue_cat_2 == "Fatigue" ~ 1,
                                                      TRUE ~ .data$complex_2_fatigue_sum),
           complex_2_fatigue_sum_cfq11 = case_when(.data$complex_2_fatigue_sum == 1 | .data$ecu_cfq11_cat == "Fatigue" ~ 1,
                                                   TRUE ~ .data$complex_2_fatigue_sum),
           complex_2_fatigue_sum_all = case_when(.data$complex_2_fatigue_sum == 1 | .data$complex_2_fatigue_sum_screen == 1 | .data$complex_2_fatigue_sum_promis29 == 1 | 
                                                   .data$complex_2_fatigue_sum_cfq11 == 1 ~ 1,
                                                 TRUE ~ .data$complex_2_fatigue_sum),
           complex_3_exercise_sum_screen = case_when(.data$complex_3_exercise_sum == 1 | .data$dysp.factor == "Ja" | .data$cfs_seid_crit2.factor == "Ja" ~ 1,
                                                     TRUE ~ .data$complex_3_exercise_sum),
           complex_3_exercise_sum_proms = case_when(.data$complex_3_exercise_sum == 1 | .data$ecu_promis29_dyspnea_cat_2 == "Dyspnea" ~ 1,
                                                    TRUE ~ .data$complex_3_exercise_sum),
           complex_3_exercise_sum_all = case_when(.data$complex_3_exercise_sum == 1 | .data$complex_3_exercise_sum_screen == 1 | .data$complex_3_exercise_sum_proms == 1 ~ 1,
                                                  TRUE ~ .data$complex_3_exercise_sum),
           complex_7_chest_sum_screen = case_when(.data$complex_7_chest_sum == 1 | .data$pain_loc_chest.factor == "Ja" ~ 1,
                                                  TRUE ~ .data$complex_7_chest_sum),
           complex_8_gastro_sum_screen = case_when(.data$complex_8_gastro_sum == 1 | .data$pain_loc_abd.factor == "Ja" ~ 1,
                                                   TRUE ~ .data$complex_8_gastro_sum),
           complex_9_neuro_sum_screen = case_when(.data$complex_9_neuro_sum == 1 | .data$cfs_seid_crit5.factor == "Ja" | .data$pain_loc_head.factor == "Ja" | 
                                                    .data$pain_dn2_6.factor == "Ja" | .data$cfs_seid_crit4.factor == "Ja" ~ 1,
                                                  TRUE ~ .data$complex_9_neuro_sum),
           complex_9_neuro_sum_proms = case_when(.data$complex_9_neuro_sum == 1 | .data$ecu_promis_cogn_funct_cat_2 == "Cognitive impairments" ~ 1,
                                                 TRUE ~ .data$complex_9_neuro_sum),
           complex_9_neuro_sum_all = case_when(.data$complex_9_neuro_sum == 1 | .data$complex_9_neuro_sum_screen == 1 | .data$complex_9_neuro_sum_proms == 1 ~ 1,
                                               TRUE ~ .data$complex_9_neuro_sum),
           complex_12_sleep_sum_promis29 = case_when(.data$complex_12_sleep_sum == 1 | .data$ecu_promis29_sleep_cat_2 == "Sleep disturbance" ~ 1,
                                                     TRUE ~ .data$complex_12_sleep_sum))
  
  ecu_pcs_score_12m <- ecu_pcs_score_12m %>%
    left_join(relevante_proms, by = c(pid, "visit_label" = visit_label_var_name)) %>%
    mutate(complex_2_fatigue_sum_screen = case_when(.data$complex_2_fatigue_sum == 1 | .data$cfs.factor == "Ja" ~ 1,
                                                    TRUE ~ .data$complex_2_fatigue_sum),
           complex_2_fatigue_sum_promis29 = case_when(.data$complex_2_fatigue_sum == 1 | .data$ecu_promis29_fatigue_cat_2 == "Fatigue" ~ 1,
                                                      TRUE ~ .data$complex_2_fatigue_sum),
           complex_2_fatigue_sum_cfq11 = case_when(.data$complex_2_fatigue_sum == 1 | .data$ecu_cfq11_cat == "Fatigue" ~ 1,
                                                   TRUE ~ .data$complex_2_fatigue_sum),
           complex_2_fatigue_sum_all = case_when(.data$complex_2_fatigue_sum == 1 | .data$complex_2_fatigue_sum_screen == 1 | .data$complex_2_fatigue_sum_promis29 == 1 | 
                                                   .data$complex_2_fatigue_sum_cfq11 == 1 ~ 1,
                                                 TRUE ~ .data$complex_2_fatigue_sum),
           complex_3_exercise_sum_screen = case_when(.data$complex_3_exercise_sum == 1 | .data$dysp.factor == "Ja" | .data$cfs_seid_crit2.factor == "Ja" ~ 1,
                                                     TRUE ~ .data$complex_3_exercise_sum),
           complex_3_exercise_sum_proms = case_when(.data$complex_3_exercise_sum == 1 | .data$ecu_promis29_dyspnea_cat_2 == "Dyspnea" ~ 1,
                                                    TRUE ~ .data$complex_3_exercise_sum),
           complex_3_exercise_sum_all = case_when(.data$complex_3_exercise_sum == 1 | .data$complex_3_exercise_sum_screen == 1 | .data$complex_3_exercise_sum_proms == 1 ~ 1,
                                                  TRUE ~ .data$complex_3_exercise_sum),
           complex_7_chest_sum_screen = case_when(.data$complex_7_chest_sum == 1 | .data$pain_loc_chest.factor == "Ja" ~ 1,
                                                  TRUE ~ .data$complex_7_chest_sum),
           complex_8_gastro_sum_screen = case_when(.data$complex_8_gastro_sum == 1 | .data$pain_loc_abd.factor == "Ja" ~ 1,
                                                   TRUE ~ .data$complex_8_gastro_sum),
           complex_9_neuro_sum_screen = case_when(.data$complex_9_neuro_sum == 1 | .data$cfs_seid_crit5.factor == "Ja" | .data$pain_loc_head.factor == "Ja" | 
                                                    .data$pain_dn2_6.factor == "Ja" | .data$cfs_seid_crit4.factor == "Ja" ~ 1,
                                                  TRUE ~ .data$complex_9_neuro_sum),
           complex_9_neuro_sum_proms = case_when(.data$complex_9_neuro_sum == 1 | .data$ecu_promis_cogn_funct_cat_2 == "Cognitive impairments" ~ 1,
                                                 TRUE ~ .data$complex_9_neuro_sum),
           complex_9_neuro_sum_all = case_when(.data$complex_9_neuro_sum == 1 | .data$complex_9_neuro_sum_screen == 1 | .data$complex_9_neuro_sum_proms == 1 ~ 1,
                                               TRUE ~ .data$complex_9_neuro_sum),
           complex_12_sleep_sum_promis29 = case_when(.data$complex_12_sleep_sum == 1 | .data$ecu_promis29_sleep_cat_2 == "Sleep disturbance" ~ 1,
                                                     TRUE ~ .data$complex_12_sleep_sum))
  
  ecu_pcs_score_3m <- ecu_pcs_score_3m %>%
    filter(.data$visit_label == "3M Follow-Up") %>%
    mutate(pcs_score_sum_with_proms = if_else(.data$complex_1_chemo_sum == 1, 3.5, 0) +
             if_else(.data$complex_2_fatigue_sum_all == 1, 7.0, 0) +
             if_else(.data$complex_3_exercise_sum_all == 1, 4.0, 0) +
             if_else(.data$complex_4_pain_sum == 1, 6.5, 0) +
             if_else(.data$complex_5_ent_sum == 1, 5.5, 0) +
             if_else(.data$complex_6_cough_sum == 1, 7.0, 0) +
             if_else(.data$complex_7_chest_sum_screen == 1, 3.5, 0) +
             if_else(.data$complex_8_gastro_sum_screen == 1, 5.0, 0) +
             if_else(.data$complex_9_neuro_sum_all == 1, 6.5, 0) +
             if_else(.data$complex_10_derma_sum == 1, 2.0, 0) +
             if_else(.data$complex_11_flulike_sum == 1, 3.5, 0) +
             if_else(.data$complex_12_sleep_sum_promis29 == 1, 5.0, 0),
           pcs_score_group_with_proms = cut(.data$pcs_score_sum_with_proms,
                                            breaks= c(-Inf, 0, 10.75, 26.25, Inf ), 
                                            labels=c("0", "<=10,75", "10,75<x<=26,25", ">26,25")))
  
  ecu_pcs_score_12m <- ecu_pcs_score_12m %>%
    mutate(pcs_score_sum_with_proms = if_else(.data$complex_1_chemo_sum == 1, 3.5, 0) +
             if_else(.data$complex_2_fatigue_sum_all == 1, 7.0, 0) +
             if_else(.data$complex_3_exercise_sum_all == 1, 4.0, 0) +
             if_else(.data$complex_4_pain_sum == 1, 6.5, 0) +
             if_else(.data$complex_5_ent_sum == 1, 5.5, 0) +
             if_else(.data$complex_6_cough_sum == 1, 7.0, 0) +
             if_else(.data$complex_7_chest_sum_screen == 1, 3.5, 0) +
             if_else(.data$complex_8_gastro_sum_screen == 1, 5.0, 0) +
             if_else(.data$complex_9_neuro_sum_all == 1, 6.5, 0) +
             if_else(.data$complex_10_derma_sum == 1, 2.0, 0) +
             if_else(.data$complex_11_flulike_sum == 1, 3.5, 0) +
             if_else(.data$complex_12_sleep_sum_promis29 == 1, 5.0, 0),
           pcs_score_group_with_proms = cut(.data$pcs_score_sum_with_proms,
                                            breaks= c(-Inf, 0, 10.75, 26.25, Inf ), 
                                            labels=c("0", "<=10,75", "10,75<x<=26,25", ">26,25")))
  
  ecu_pcs_score <- ecu_pcs_score_3m %>%
    full_join(ecu_pcs_score_12m) %>%
    select(all_of(pid), "visit_label", contains("pcs_score"))
  
  return(ecu_pcs_score)
  
}


## ARDS ========================================================================

#' Build dataframe with ARDS by imaging procedures
#' 
#' @return dataframe, which contains the ards data
#' 
#' @param trial_data A secuTrial data object
#' @param pid column name of patient ID in trial_data
#' @importFrom rlang .data
#' @export

build_ards_df <- function (trial_data, pid) {
  
  death_yes <- "a Tod"
  ards_yes <- "b ARDS Kriterien erf\u00fcllt"
  ards_excl <- "c ARDS ausgeschlossen"
  patho_yes <- "c Pathologischer Befund"
  imaging_no <- "f Gar keine Bildgebung"
  normal_res <- "d Normalbefund"
  no_res <- "e Kein Befund"
  no_info <- "e Keine Informationen verf\u00fcgbar"
  imaging_no_further <- "j Keine weitere Bildgebung"
  result_unclear <- "k Befund unklar"
  
  # ARDS via imaging
  lung_diag <- trial_data$fv13 %>%
    mutate(form_parent = "fv13") %>%
    select(all_of(pid),"form_parent", "gec_ct.factor", "gec_xray.factor", "gec_lus.factor", "lmr.factor") 
  
  # CT Thorax
  ect_ards <- trial_data$ect %>% 
    select(all_of(pid),"gec_ct_result.factor", "ct_date.date", "ct_date_uk.factor", "gec_ct_covid19", "ct_cons", "ct_cons_side", "ct_ret", "ct_grglass", 
           "ct_grglass_side", "ct_crazypav", "ct_treeinb", "ct_treeinb_side") %>%
    rename(result.factor = "gec_ct_result.factor",
           outcome_date.date = "ct_date.date",
           outcome_date_uk.factor= "ct_date_uk.factor") %>%
    mutate(form = "ect") %>%
    filter(!is.na(.data$result.factor))
  
  # X-Ray Thorax
  exray_ards <- trial_data$exray %>%
    select(all_of(pid),"gec_xray_result.factor", "xray_date.date", "xray_date_uk.factor", "xray_cons", "xray_cons_side", "xray_infil", "xray_infil_side") %>%
    rename(result.factor = "gec_xray_result.factor",
           outcome_date.date = "xray_date.date",
           outcome_date_uk.factor = "xray_date_uk.factor") %>%
    mutate(form = "exray") %>%
    filter(.data$result.factor !="Keine Informationen verf\u00fcgbar" | !is.na(.data$result.factor))
  
  # US Thorax
  elus_ards <- trial_data$elus %>% 
    select(all_of(pid),"gec_lus_result.factor", "lus_date.date", "lus_date_uk.factor") %>%
    rename(result.factor = "gec_lus_result.factor",
           outcome_date.date = "lus_date.date",
           outcome_date_uk.factor = "lus_date_uk.factor") %>%
    mutate(form = "elus")%>%
    filter(.data$result.factor !="Keine Informationen verf\u00fcgbar" | !is.na(.data$result.factor))
  
  #MRI Thorax
  elmr_ards <- trial_data$elmr %>% 
    select(all_of(pid),"lmr_result.factor", "lmr_date.date", "lmr_date_uk.factor") %>%
    rename(result.factor = "lmr_result.factor",
           outcome_date.date = "lmr_date.date",
           outcome_date_uk.factor = "lmr_date_uk.factor") %>%
    mutate(form = "elmr") %>%
    filter(.data$result.factor !="Keine Informationen verf\u00fcgbar"| !is.na(.data$result.factor))
  
  img_ards <- ect_ards %>%
    bind_rows(exray_ards) %>%
    bind_rows(elus_ards) %>%
    bind_rows(elmr_ards) 
  
  ards <- lung_diag %>%
    left_join(img_ards, by = pid) %>%
    mutate(form = ifelse(is.na(.data$form), .data$form_parent, .data$form),
           result.factor = as.character(.data$result.factor),
           result.factor = case_when(str_detect(.data$result.factor, "Normalbefund") ~ normal_res,
                                     str_detect(.data$result.factor, "Pathologisch") ~ patho_yes,
                                     str_detect(.data$result.factor, "Kein Befund") ~ no_res,
                                     str_detect(.data$result.factor, "Nein, kein")  ~ imaging_no_further,
                                     str_detect(.data$result.factor, "Keine Informationen verf\u00fcgbar")  ~ no_info,
                                     .data$gec_ct.factor == "Nein, kein CT-Thorax" & 
                                       .data$gec_xray.factor == "Nein, kein R\u00f6ntgen-Thorax" &
                                       .data$gec_lus.factor == "Nein, kein Lungenultraschall" & 
                                       .data$lmr.factor == "Nein, keine MRT-Untersuchung der Lunge" ~ imaging_no,
                                     .data$gec_ct.factor == "Keine Informationen verf\u00fcgbar" & 
                                       .data$gec_xray.factor == "Keine Informationen verf\u00fcgbar" & 
                                       .data$gec_lus.factor == "Keine Informationen verf\u00fcgbar" & 
                                       .data$lmr.factor == "Keine Informationen verf\u00fcgbar" ~ no_info,
                                     !is.na(.data$result.factor) ~ .data$result.factor,
                                     TRUE ~ result_unclear),
           outcome = case_when(.data$gec_ct_covid19 == 1 | .data$gec_ct_covid19 == 2 ~ ards_yes,
                               .data$ct_cons == 1 & .data$ct_cons_side == 3  ~ ards_yes,
                               .data$ct_ret == 1 & (.data$ct_grglass == 1 | .data$ct_crazypav == 1) ~ ards_yes,
                               .data$ct_grglass == 1 & .data$ct_grglass_side == 3  ~ ards_yes,
                               .data$ct_treeinb == 1 & .data$ct_treeinb_side == 3 ~ ards_yes,
                               # explicitly exclude ards
                               .data$gec_ct_covid19 != 4 & 
                                 (.data$ct_cons !=1 | (.data$ct_cons == 1 & .data$ct_cons_side != 3 & .data$ct_cons_side != 4)) &
                                 (.data$ct_ret !=1 | (.data$ct_ret == 1 & .data$ct_grglass != 3 & .data$ct_crazypav != 3)) &
                                 (.data$ct_grglass ==1 & .data$ct_grglass_side !=3 & .data$ct_grglass_side !=4) &
                                 (.data$ct_treeinb != 1 | (.data$ct_treeinb == 1 & .data$ct_treeinb_side !=3 & .data$ct_treeinb_side !=4)) ~ ards_excl,
                               TRUE ~ as.character(.data$result.factor)),
           # Examination date is exact
           outcome_date_d.factor = ifelse(!is.na(.data$outcome_date.date), "Exakte Angabe (Befunddatum)", NA),
           ards_rx = case_when(.data$xray_cons == 1 & .data$xray_cons_side == 3 ~ ards_yes,
                               .data$xray_infil == 1 & .data$xray_infil_side == 3 ~ ards_yes,
                               # explicitly exclude ards
                               (.data$xray_cons != 1 | (.data$xray_cons ==1 & .data$xray_cons_side != 3 & .data$xray_cons_side != 4)) &
                                 (.data$xray_infil != 1 | (.data$xray_infil ==1 & .data$xray_infil_side != 3 & .data$xray_infil_side != 4))  ~ ards_excl,
                               TRUE ~ as.character(.data$result.factor)),
           outcome = ifelse(.data$form == "exray", .data$ards_rx, .data$outcome)) %>%
    select(all_of(pid), "result.factor", "outcome", "outcome_date.date", "outcome_date_d.factor", "outcome_date_uk.factor", 
           "form", "gec_ct.factor", "gec_xray.factor", "gec_lus.factor", "lmr.factor") 
  
  return(ards)
  
}


## EQ5D for children ===========================================================

#' Calculate EQ5D-3L-Index (PROXY Version)
#' 
#' @description Calculates EQ-5D-3L index following recommendations for Germany
#' @param mo is a vector for mobility score of eq5d
#' @param sc is a vector for self-care score of eq5d
#' @param ua is a vector for usual activity score of eq5d
#' @param pd is a vector for pain/discomfort score of eq5d
#' @param ad is a vector for anxiety/depression score of eq5d
#' @importFrom eq5d eq5d
#' @return vector with eq5d-3l-index-proxy 
#' @export

calculate_eq5d3l_proxy_index <- function (mo, sc, ua, pd, ad) {
  ecu_eq5d_score <- paste (mo, sc, ua, pd, ad, sep="")
  ecu_eq5d3l_proxy_index <- eq5d::eq5d(scores = ecu_eq5d_score, type = "TTO", version = "3L", country = "Germany", ignore.invalid = TRUE)
  
  return (ecu_eq5d3l_proxy_index)
}


#' Calculate EQ5D-3L-Index (Y Version)
#' 
#' @description Calculates EQ-5D-3L index following recommendations for Germany
#' @param mo is a vector for mobility score of eq5d
#' @param sc is a vector for self-care score of eq5d
#' @param ua is a vector for usual activity score of eq5d
#' @param pd is a vector for pain/discomfort score of eq5d
#' @param ad is a vector for anxiety/depression score of eq5d
#' @importFrom eq5d eq5d
#' @return vector with eq5d-3l-index-y 
#' @export

calculate_eq5d3l_y_index <- function (mo, sc, ua, pd, ad) {
  ecu_eq5d_score <- paste (mo, sc, ua, pd, ad, sep="")
  ecu_eq5d3l_y_index <- eq5d::eq5d(scores = ecu_eq5d_score, type = "TTO", version = "Y", country = "Germany", ignore.invalid = TRUE)
  
  return (ecu_eq5d3l_y_index)
}


#' Calculate EQ5D-5L-Index (Y Version)
#' 
#' @description Calculates EQ-5D-5L index following recommendations for Germany
#' @param mo is a vector for mobility score of eq5d
#' @param sc is a vector for self-care score of eq5d
#' @param ua is a vector for usual activity score of eq5d
#' @param pd is a vector for pain/discomfort score of eq5d
#' @param ad is a vector for anxiety/depression score of eq5d
#' @importFrom eq5d eq5d
#' @return vector with eq5d-5l-index-y 
#' @export

calculate_eq5d5l_y_index <- function (mo, sc, ua, pd, ad) {
  ecu_eq5d_score <- paste (mo, sc, ua, pd, ad, sep="")
  ecu_eq5d5l_y_index <- eq5d::eq5d(scores = ecu_eq5d_score, type = "VT", version = "Y", country = "Germany", ignore.invalid = TRUE)
  
  return (ecu_eq5d5l_y_index)
}


## Helper Functions for Post-COVID-Syndrome Score (PCS) - Handling Meta Data ========

#' Return parent table name (for one parent table)
#' 
#' @param parentformtablename A vector with parent table name
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data

return_child_table_names_of_parent <- function(parentformtablename, trial_data){
  
  trial_data$qs %>%
    filter(!is.na(.data$subformtablename) & str_ends(.data$formtablename,  parentformtablename)) %>%
    group_by(.data$subformtablename) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(subformtablename = str_remove_all(.data$subformtablename, "mnpnum1")) %>%
    pull(.data$subformtablename)
  
}


#' Return parent table names (for two or more parent tables)
#'  
#' @param parentformtablenames A vector with parent table names
#' @param trial_data A SecuTrial data object

return_children_table_names_of_parents <- function(parentformtablenames, trial_data){
  
  subformtablenames <- character(0)
  
  for (parentformtablename in parentformtablenames) {
    subformtablenames_curr <- return_child_table_names_of_parent(parentformtablename, trial_data)
    subformtablenames <- append(subformtablenames_curr, subformtablenames)
  }
  
  return(subformtablenames)
}


#' Return parent table names (for two or more parent tables)
#' 
#' @param trial_data A SecuTrial data object
#' @importFrom rlang .data

return_parent_child_pairs <- function(trial_data){
  # parentformtablename <- "fv2_1"
  # subformtablename <- "esmell"
  # subformtablename_to_filter <- subformtablename
  
  parent_child_pairs_meta <-  trial_data$qs %>%
    mutate(subformtablename = str_remove_all(.data$subformtablename, "mnpnum1"),
           formtablename = str_remove_all(.data$formtablename, "mnpnum1")) %>%
    filter(!is.na(.data$subformtablename)) %>%
    group_by(.data$subformtablename) %>%
    filter(row_number() == 1) %>%
    ungroup()  %>%
    select("formtablename", "formname", "subformtablename", "fglabel")
  
  return(parent_child_pairs_meta)
  
}


## Helper Functions for Symptom Data =============================================

#' Build symptom subform data frame
#' 
#' @param symptom_parentformtablenames A vector containing parent table names for symptoms
#' @param trial_data A SecuTrial data object
#' @param pid column name of patient ID in data
#' @importFrom rlang .data

build_sy_subform_merged_df <- function(symptom_parentformtablenames, trial_data, pid) {
  
  symptom_subformtablenames <-  return_children_table_names_of_parents(symptom_parentformtablenames, trial_data)
  
  parent_child_pairs_meta <- return_parent_child_pairs(trial_data)
  
  # subset object for subformtables, unify same meaning variables in those tables and combine them to a single dataframe 
  sy_subforms_merged <- trial_data[symptom_subformtablenames] %>%
    modify_at("esyoth", ~ filter(.x, .data$sy_oth_icd11name == "Fatigue" |  .data$sy_oth_icd11name == "Tiredness")) %>%
    #unify date relevant variable names with same meaning for all symptom subforms (.x is a placeholder for a single subformname)
    modify(~ rename_at(.x, vars(ends_with("_start.date")),  ~ "sy_start.date")) %>%
    modify(~ rename_at(.x, vars(ends_with("_d.factor")),  ~ "sy_d.factor")) %>%
    modify(~ rename_at(.x, vars(ends_with("_start_uk.factor")),  ~ "sy_start_uk.factor")) %>%
    modify(~ rename_at(.x, vars(ends_with("_dur.factor")),  ~ "sy_dur.factor")) %>%
    # extract the variable name within each subform, that ends with "_start" (and remove "_start") as a string to a new column;
    # for most symptoms the extracted name corresponds to the filtering variable name in the parentform, 
    # which contains "Yes/No" etc. -> we will need the "No's" later, too
    modify(~ mutate(.x, sy_extracted_name_child = str_remove(names(.)[which(str_ends(names(.),"_start"))], "_start"))) %>%
    # now stack all subforms into one dataframe; 
    # same variable names will be kept in same column thanks to the renaming in the previous step
    bind_rows(.id="subformtablename") %>%
    mutate(sy_extracted_name_child = str_replace(.data$sy_extracted_name_child, "sy_pd_coug", "sy_pd_cough")) %>%
    left_join(parent_child_pairs_meta, by = "subformtablename") %>%
    select(all_of(pid),"formtablename", "formname", "subformtablename", "fglabel", "sy_extracted_name_child", "position",  
           "sy_oth_icd11name", "sy_start.date", "sy_d.factor", "sy_start_uk.factor", "sy_dur.factor") %>%
    # join visit data
    filter(if_any(c("sy_start.date", "sy_d.factor", "sy_start_uk.factor", "sy_dur.factor"), ~ !is.na(.))) 
  
  return(sy_subforms_merged)
  
}


#' Build symptom subform data frame (merged with parent forms)
#' 
#' @param symptom_parentformtablenames A vector containing parent table names for symptoms
#' @param trial_data A SecuTrial data object
#' @param pid column name of patient ID in data
#' @importFrom rlang .data

build_sy_parentform_merged_df <- function(symptom_parentformtablenames, trial_data, pid) {
  
  parent_meta <- return_parent_child_pairs(trial_data) %>%
    group_by(.data$formtablename) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select("formtablename", "formname")
  
  # subset object for formtables and combine them to a single dataframe 
  sy_parentforms_merged <- trial_data[symptom_parentformtablenames] %>%
    modify( ~ .x %>% select(starts_with("sy"), starts_with("gec_sy"),  all_of(pid)))%>%
    # populate the answer of the main filtering question into the branching questions, if it's not a "Ja", because they are hidden and therefore empty
    modify_at("fv3_1",  ~ .x %>% mutate(across(ends_with(".factor") & !starts_with("p_sy"), ~ if_else(gec_sy.factor!= "Ja",  gec_sy.factor, .))))%>%
    modify_at("fv3_2",  ~ .x %>% mutate(across(ends_with(".factor") & !starts_with("p_sy") & contains("sy_pd"), ~ if_else(sy_pd.factor != "Ja",  sy_pd.factor, .))))%>%
    modify_at("fv3_2",  ~ .x %>% mutate(across(ends_with(".factor") & !starts_with("p_sy") & contains("sy_gi"), ~ if_else(sy_gi.factor != "Ja",  sy_gi.factor, .))))%>%
    modify_at("fv3_3",  ~ .x %>% mutate(across(ends_with(".factor") & !starts_with("p_sy") & contains("sy_ne"), ~ if_else(sy_ne.factor != "Ja",  sy_ne.factor, .))))%>%
    # now reshape each table into long format
    modify(~ pivot_longer(.x, cols =  ends_with("factor"), names_to = "sy_extracted_name_parent", values_to = "sy_parent")) %>%
    # reduce(full_join, by=c(pid, "not_valid_for_3M_Follow_Up", "not_valid_for_12M_Follow_Up"))
    bind_rows(.id="formtablename") %>%
    mutate(sy_extracted_name_parent = str_remove(.data$sy_extracted_name_parent, ".factor")) %>%
    mutate(sy_extracted_name_parent_without_gec = str_remove(.data$sy_extracted_name_parent, "gec_")) %>%
    left_join(parent_meta, by = "formtablename") %>%
    select("formtablename", "formname", all_of(pid), "sy_extracted_name_parent", "sy_extracted_name_parent_without_gec", "sy_parent", ends_with("factor"))
  
  return(sy_parentforms_merged)
  
}


#' Build symptom subform data frame (merged with parent and cild forms)
#' 
#' @param trial_data A SecuTrial data object
#' @param pid column name of patient ID in data
#' @importFrom rlang .data

build_sy_parents_children_merged_df <- function(trial_data, pid){
  
  symptom_parentformtablenames <- c("fv3_1", "fv3_2", "fv3_3")
  
  sy_subforms_merged <- build_sy_subform_merged_df(symptom_parentformtablenames, trial_data, pid)
  
  #TODO: fuer tableOne, spalten aus fglabel ja/nein bauen
  # sy_subforms_merged %>%
  #  pivot_wider()
  
  # TODO: missing type -1 (keine Information verfuegbar) -> NA
  
  # TODO: transfer analyses steps into new script
  #look at all symptoms, take the top5 most frequently ones for further analysis
  
  sy_parentforms_merged <- build_sy_parentform_merged_df(symptom_parentformtablenames, trial_data, pid) 
  
  sy_parents_and_children <- sy_parentforms_merged %>%
    full_join(sy_subforms_merged, 
              by = c("formtablename", "formname", pid, "sy_extracted_name_parent_without_gec" = "sy_extracted_name_child")) %>%
    # discard paediatric data
    # filter(str_starts(sy_extracted_name_parent, "sy")) %>% #MK: replaced 
    filter(!str_starts(.data$sy_extracted_name_parent, "p_")) %>%
    # the following filter helps to check, if all subforms were joined with their parent forms
    #TODO: this parents remain without children "sy_gi_oth" "sy_pd_oth" "sy_ne_oth" "sy_oth". Need to define ICDs, if to include.
    #  mutate(filtering_var = sy_extracted_name_parent %in% c("gec_sy","sy_pd", "sy_gi","sy_ne")) %>%
    # filter(!filtering_var & is.na(subformtablename) & sy_parent == "Ja")
    select(all_of(pid),"formtablename", "formname", "sy_parent", "subformtablename", "fglabel", "sy_extracted_name_parent",
           "sy_oth_icd11name", matches("\\.date$|\\.factor$")) %>%
    mutate(sy_family = 
             case_when(str_detect(.data$sy_extracted_name_parent, "_pd") ~ "respiratory",
                       str_detect(.data$sy_extracted_name_parent, "_gi") ~ "gastro",
                       str_detect(.data$sy_extracted_name_parent, "_ne") | str_detect(.data$sy_oth_icd11name, "Fatigue|Tiredness") ~ "neuro",
                       str_detect(.data$formtablename, "fv3_1") ~ "general"
             ))
  
  return(sy_parents_and_children)
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

calculate_promis_29_fatigue_sum <- function(pro_fatigue_1, pro_fatigue_2, pro_fatigue_3, pro_fatigue_4) {
  
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

categorize_promis_29_fatigue <- function(pro_fatigue_sum) {
  
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

categorize_promis_29_fatigue_2 <- function(pro_fatigue_sum) {
  
  pro_fatigue_cat_2 <- case_when(pro_fatigue_sum < 11 ~ "No Fatigue",
                                 pro_fatigue_sum >= 11 ~ "Fatigue")
  
  return(pro_fatigue_cat_2)
  
}


#' Primary coding PROMIS-29 Fatigue
#' 
#' adds the following columns to promext:
#' ecu_promis29_fatigue_sum, ecu_promis29_fatigue_cat, ecu_promis29_fatigue_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @noRd

primary_coding_suep_promis_29_fatigue <- function(trial_data, visitid) {
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_promis29_fatigue_sum = calculate_promis_29_fatigue_sum(.data$pro_29_hi7, .data$pro_29_an3, .data$pro_29_fatexp41, .data$pro_29_fatexp40),
           ecu_promis29_fatigue_cat = categorize_promis_29_fatigue(.data$ecu_promis29_fatigue_sum),
           ecu_promis29_fatigue_cat_2 = categorize_promis_29_fatigue_2(.data$ecu_promis29_fatigue_sum)) %>%
    select(matches(visitid), "ecu_promis29_fatigue_sum", "ecu_promis29_fatigue_cat", "ecu_promis29_fatigue_cat_2")
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


### PROMIS-29 Dyspnea ==========================================================

#' Recode PROMIS-29 Dyspnea items
#' 
#' @description
#' recodes the PROMIS-29 Dyspnea items to needed levels
#' 
#' @param pro_dysp_var.factor Item of PROMIS-29 Dyspnea that needs to be recoded
#' 
#' @return A vector with the correctly coded PROMIS-29 Dyspnea item
#' @export

recode_promis_dyspnoe <- function(pro_dysp_var.factor) {
  
  case_when(pro_dysp_var.factor == "Keine Schwierigkeiten" ~ 0, 
            pro_dysp_var.factor == "Leichte Schwierigkeiten" ~ 1, 
            pro_dysp_var.factor == "Einige Schwierigkeiten" ~ 2, 
            pro_dysp_var.factor == "Gro\u00dfe Schwierigkeiten" ~ 3, 
            TRUE ~ NA_real_)
}


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
  
  pro_dysp_n <- ifelse(!is.na(pro_dysp_1), 1, 0) +
    ifelse(!is.na(pro_dysp_2), 1, 0) +
    ifelse(!is.na(pro_dysp_3), 1, 0) +
    ifelse(!is.na(pro_dysp_4), 1, 0) +
    ifelse(!is.na(pro_dysp_5), 1, 0) +
    ifelse(!is.na(pro_dysp_6), 1, 0) +
    ifelse(!is.na(pro_dysp_7), 1, 0) +
    ifelse(!is.na(pro_dysp_8), 1, 0) +
    ifelse(!is.na(pro_dysp_9), 1, 0) +
    ifelse(!is.na(pro_dysp_10), 1, 0)
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
  
  pro_dysp_sum <- ifelse(is.na(pro_dysp_1), 0, pro_dysp_1) +
    ifelse(is.na(pro_dysp_2), 0, pro_dysp_2) +
    ifelse(is.na(pro_dysp_3), 0, pro_dysp_3) +
    ifelse(is.na(pro_dysp_4), 0, pro_dysp_4) + 
    ifelse(is.na(pro_dysp_5), 0, pro_dysp_5) +
    ifelse(is.na(pro_dysp_6), 0, pro_dysp_6) + 
    ifelse(is.na(pro_dysp_7), 0, pro_dysp_7) +
    ifelse(is.na(pro_dysp_8), 0, pro_dysp_8) +
    ifelse(is.na(pro_dysp_9), 0, pro_dysp_9) + 
    ifelse(is.na(pro_dysp_10), 0, pro_dysp_10)
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
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @noRd

primary_coding_suep_promis_29_dyspnea <- function(trial_data, visitid) {
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  if (! "ecu_pro_dysp_1" %in% names(form_to_add_vars)) {
    
    new_vars_to_add <- form_to_add_vars %>%
      mutate(pro_dysp_1 = recode_promis_dyspnoe(.data$pro_dysfl001.factor), 
             pro_dysp_2 = recode_promis_dyspnoe(.data$pro_dysfl002.factor),
             pro_dysp_3 = recode_promis_dyspnoe(.data$pro_dysfl003.factor),
             pro_dysp_4 = recode_promis_dyspnoe(.data$pro_dysfl004.factor),
             pro_dysp_5 = recode_promis_dyspnoe(.data$pro_dysfl005.factor),
             pro_dysp_6 = recode_promis_dyspnoe(.data$pro_dysfl006.factor),
             pro_dysp_7 = recode_promis_dyspnoe(.data$pro_dysfl007.factor),
             pro_dysp_8 = recode_promis_dyspnoe(.data$pro_dysfl008.factor),
             pro_dysp_9 = recode_promis_dyspnoe(.data$pro_dysfl009.factor),
             pro_dysp_10 = recode_promis_dyspnoe(.data$pro_dysfl10.factor),
             ecu_promis29_dyspnea_n = count_n_promis_29_dyspnea(.data$pro_dysp_1, .data$pro_dysp_2, .data$pro_dysp_3, .data$pro_dysp_4, .data$pro_dysp_5, 
                                                                .data$pro_dysp_6, .data$pro_dysp_7, .data$pro_dysp_8, .data$pro_dysp_9, .data$pro_dysp_10),
             ecu_promis29_dyspnea_sum = calculate_promis_29_dyspnea_sum(.data$pro_dysp_1, .data$pro_dysp_2, .data$pro_dysp_3, .data$pro_dysp_4, .data$pro_dysp_5, 
                                                                        .data$pro_dysp_6,.data$pro_dysp_7, .data$pro_dysp_8, .data$pro_dysp_9, .data$pro_dysp_10, 
                                                                        .data$ecu_promis29_dyspnea_n),
             ecu_promis29_dyspnea_cat = categorize_promis_29_dyspnea(.data$ecu_promis29_dyspnea_sum),
             ecu_promis29_dyspnea_cat_2 = categorize_promis_29_dyspnea_2(.data$ecu_promis29_dyspnea_sum)) %>%
      select(matches(visitid), "ecu_promis29_dyspnea_n", "ecu_promis29_dyspnea_sum", "ecu_promis29_dyspnea_cat", "ecu_promis29_dyspnea_cat_2") } else {
        
        new_vars_to_add <- form_to_add_vars %>%
          mutate(ecu_promis29_dyspnea_n = count_n_promis_29_dyspnea(.data$ecu_pro_dysp_1, .data$ecu_pro_dysp_2, .data$ecu_pro_dysp_3, .data$ecu_pro_dysp_4, .data$ecu_pro_dysp_5, 
                                                                    .data$ecu_pro_dysp_6, .data$ecu_pro_dysp_7, .data$ecu_pro_dysp_8, .data$ecu_pro_dysp_9, .data$ecu_pro_dysp_10),
                 ecu_promis29_dyspnea_sum = calculate_promis_29_dyspnea_sum(.data$ecu_pro_dysp_1, .data$ecu_pro_dysp_2, .data$ecu_pro_dysp_3, .data$ecu_pro_dysp_4, .data$ecu_pro_dysp_5, 
                                                                            .data$ecu_pro_dysp_6, .data$ecu_pro_dysp_7, .data$ecu_pro_dysp_8, .data$ecu_pro_dysp_9, .data$ecu_pro_dysp_10, 
                                                                            .data$ecu_promis29_dyspnea_n),
                 ecu_promis29_dyspnea_cat = categorize_promis_29_dyspnea(.data$ecu_promis29_dyspnea_sum),
                 ecu_promis29_dyspnea_cat_2 = categorize_promis_29_dyspnea_2(.data$ecu_promis29_dyspnea_sum)) %>%
          select(matches(visitid), "ecu_promis29_dyspnea_n", "ecu_promis29_dyspnea_sum", "ecu_promis29_dyspnea_cat", "ecu_promis29_dyspnea_cat_2") 
        
      }
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


### PROMIS Cognitive Impairments ===============================================

#' Recode PROMIS-29 Cognitive impairments items
#' 
#' @description 
#' recodes the PROMIS-29 Cognitive impairments items to needed levels
#' 
#' @param pro_cogn_var.factor Item of PROMIS-29 Cognitive impairments that needs to be recoded
#' 
#' @return A vector with the correctly coded PROMIS-29 Cognitive impairments item
#' @export

recode_promis_cognitive <- function(pro_cogn_var.factor) {
  
  case_when(pro_cogn_var.factor == "Nie" ~ 5, 
            pro_cogn_var.factor == "Selten (einmal)" ~ 4, 
            pro_cogn_var.factor == "Manchmal (zwei- oder dreimal)" ~ 3, 
            pro_cogn_var.factor == "Oft (ungef\u00e4hr einmal t\u00e4glich)" ~ 2, 
            pro_cogn_var.factor == "Sehr oft (mehrmals t\u00e4glich)" ~ 1,
            TRUE ~ NA_real_)
}


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

calculate_promis_cognitive_funct_sum <- function(pro_cogn_1, pro_cogn_2, pro_cogn_3, pro_cogn_4) {
  
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

categorize_promis_cognitive_funct <- function(pro_cogn_sum) {
  
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

categorize_promis_cognitive_funct_2 <- function(pro_cogn_sum) {
  
  pro_cogn_cat_2 <- case_when(pro_cogn_sum < 15 ~ "Cognitive impairments",
                              pro_cogn_sum >= 15 ~ "No cognitive impairments")
  
  return(pro_cogn_cat_2)
  
}


#' Primary coding PROMIS-29 Cognitive impairments
#' 
#' adds the following columns to prom:
#' ecu_promis_cogn_funct_sum, ecu_promis_cogn_funct_cat, ecu_promis_cogn_funct_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @noRd

primary_coding_suep_promis_cogn_funct <- function(trial_data, visitid) {
  
  formname_to_add_vars <- "prom"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  if (! "ecu_pro_cogn_1" %in% names(form_to_add_vars)) {
    
    new_vars_to_add <- form_to_add_vars %>%
      mutate(pro_cogn_1 = recode_promis_cognitive(.data$pro_pc2r.factor), 
             pro_cogn_2 = recode_promis_cognitive(.data$pro_pc35r.factor),
             pro_cogn_3 = recode_promis_cognitive(.data$pro_pc36r.factor),
             pro_cogn_4 = recode_promis_cognitive(.data$pro_pc42r.factor),
             ecu_promis_cogn_funct_sum = calculate_promis_cognitive_funct_sum(.data$pro_cogn_1, .data$pro_cogn_2, .data$pro_cogn_3, .data$pro_cogn_4),
             ecu_promis_cogn_funct_cat = categorize_promis_cognitive_funct(.data$ecu_promis_cogn_funct_sum),
             ecu_promis_cogn_funct_cat_2 = categorize_promis_cognitive_funct_2(.data$ecu_promis_cogn_funct_sum)) %>%
      select(matches(visitid), "ecu_promis_cogn_funct_sum", "ecu_promis_cogn_funct_cat", "ecu_promis_cogn_funct_cat_2") } else {
        
        new_vars_to_add <- form_to_add_vars %>%
          mutate(ecu_promis_cogn_funct_sum = calculate_promis_cognitive_funct_sum(.data$ecu_pro_cogn_1, .data$ecu_pro_cogn_2, .data$ecu_pro_cogn_3, .data$ecu_pro_cogn_4),
                 ecu_promis_cogn_funct_cat = categorize_promis_cognitive_funct(.data$ecu_promis_cogn_funct_sum),
                 ecu_promis_cogn_funct_cat_2 = categorize_promis_cognitive_funct_2(.data$ecu_promis_cogn_funct_sum)) %>%
          select(matches(visitid), "ecu_promis_cogn_funct_sum", "ecu_promis_cogn_funct_cat", "ecu_promis_cogn_funct_cat_2")
        
      }
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


### PROMIS Sleep disturbance ===================================================

#' Recode PROMIS-29 Sleep disturbance items
#' 
#' @description 
#' recodes the PROMIS-29 Sleep disturbance items to needed levels
#' 
#' @param pro_sleep_var.factor Item of PROMIS-29 Sleep disturbance that needs to be recoded
#' 
#' @return A vector with the correctly coded PROMIS-29 Sleep disturbance item
#' @export

recode_promis29_sleep <- function(pro_sleep_var.factor) {
  
  case_when(pro_sleep_var.factor == "Sehr schlecht" | pro_sleep_var.factor == "\u00dcberhaupt nicht" ~ 5, 
            pro_sleep_var.factor == "Schlecht" | pro_sleep_var.factor == "Ein wenig" ~ 4, 
            pro_sleep_var.factor == "M\u00e4\u00dfig" ~ 3, 
            pro_sleep_var.factor == "Gut" | pro_sleep_var.factor == "Ziemlich" ~ 2, 
            pro_sleep_var.factor == "Sehr gut" | pro_sleep_var.factor == "Sehr" ~ 1,
            TRUE ~ NA_real_)
}


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

calculate_promis29_sleep_sum <- function(pro_sleep_1, pro_sleep_2, pro_sleep_3, pro_sleep_4) {
  
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

categorize_promis29_sleep <- function(pro_sleep_sum) {
  
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

categorize_promis29_sleep_2 <- function(pro_sleep_sum) {
  
  pro_sleep_cat_2 <- case_when(pro_sleep_sum < 13 ~ "No sleep disturbance",
                               pro_sleep_sum >= 12 ~ "Sleep disturbance")
  
  return(pro_sleep_cat_2)
  
}


#' Primary coding PROMIS-29 Sleep disturbance
#' 
#' adds the following columns to promext:
#' ecu_promis29_sleep_sum, ecu_promis29_sleep_cat, ecu_promis29_sleep_cat_2
#' 
#' @param trial_data A secuTrial data object
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @noRd

primary_coding_suep_promis_29_sleep <- function(trial_data, visitid) {
  
  formname_to_add_vars <- "promext"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]
  
  if (! "ecu_pro29_sleep_1" %in% names(form_to_add_vars)) {
    
    new_vars_to_add <- form_to_add_vars %>%
      mutate(pro29_sleep_1 = recode_promis29_sleep(.data$pro_29_sleep109.factor), 
             pro29_sleep_2 = recode_promis29_sleep(.data$pro_29_sleep116.factor),
             ecu_promis29_sleep_sum = calculate_promis29_sleep_sum(.data$pro29_sleep_1, .data$pro29_sleep_2, .data$pro_29_sleep20, .data$pro_29_sleep44),
             ecu_promis29_sleep_cat = categorize_promis29_sleep(.data$ecu_promis29_sleep_sum),
             ecu_promis29_sleep_cat_2 = categorize_promis29_sleep_2(.data$ecu_promis29_sleep_sum)) %>%
      select(matches(visitid), "ecu_promis29_sleep_sum", "ecu_promis29_sleep_cat", "ecu_promis29_sleep_cat_2") } else {
        
        new_vars_to_add <- form_to_add_vars %>%
          mutate(ecu_promis29_sleep_sum = calculate_promis29_sleep_sum(.data$ecu_pro29_sleep_1, .data$ecu_pro29_sleep_2, .data$pro_29_sleep20, .data$pro_29_sleep44),
                 ecu_promis29_sleep_cat = categorize_promis29_sleep(.data$ecu_promis29_sleep_sum),
                 ecu_promis29_sleep_cat_2 = categorize_promis29_sleep_2(.data$ecu_promis29_sleep_sum)) %>%
          select(matches(visitid), "ecu_promis29_sleep_sum", "ecu_promis29_sleep_cat", "ecu_promis29_sleep_cat_2")
        
      }
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  return(trial_data)
  
}


### PROMIS Physical Function ===================================================

#' Recode PROMIS-29 Physical Function items
#' 
#' @description 
#' recodes the PROMIS-29 Physical Function items to needed levels
#' 
#' @param pro_phys_funct.factor Item of PROMIS-29 Physical Function that needs to be recoded
#' 
#' @return A vector with the correctly coded PROMIS-29 Physical Function item
#' @export

recode_promis29_phys_funct <- function(pro_phys_funct.factor) {
  case_when(pro_phys_funct.factor == "Ohne jede Schwierigkeit" ~ 5,
            pro_phys_funct.factor == "Mit geringen Schwierigkeiten" ~ 4,
            pro_phys_funct.factor == "Mit einigen Schwierigkeiten" ~ 3, 
            pro_phys_funct.factor == "Mit gro\u00dfen Schwierigkeiten" ~ 2,
            pro_phys_funct.factor == "Kann ich gar nicht" ~ 1,
            TRUE ~ NA_real_)
}

### PROMIS-29 Ability to participate in social roles and activities ============

#' Recode PROMIS-29 Ability to participate in social roles and activities items
#' 
#' @description 
#' recodes the PROMIS-29 Ability to participate in social roles and activities items to needed levels
#' 
#' @param pro_social.factor Item of PROMIS-29 Ability to participate in social roles and activities that needs to be recoded
#' 
#' @return A vector with the correctly coded PROMIS-29 Ability to participate in social roles and activities item
#' @export

recode_promis29_social <- function(pro_social.factor) {
  
  case_when(pro_social.factor == "Nie" ~ 5,
            pro_social.factor == "Selten" ~ 4,
            pro_social.factor == "Manchmal" ~ 3, 
            pro_social.factor == "Oft" ~ 2,
            pro_social.factor == "Immer" ~ 1,
            TRUE ~ NA_real_)
}