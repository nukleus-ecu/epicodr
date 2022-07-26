#################
# Primary coding for NAPKON SUEP secuTrial data via secuTrialR
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

#' Primary coding age
#'
#' adds the following columns to bv1: ecu_age - age in years, ecu_age_cat_dec - age in decades, ecu_age_cat_3 - age in 3 categories 
#'
#' @param trial_data SecuTrial Data
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
      ecu_age = calculate_full_years(from = .data$gec_birthdate.date, to = .data$gec_pr_incl_date.date),
      ecu_age_cat_dec = ecu_age_cat_dec(.data$ecu_age),
      ecu_age_cat_3 = ecu_age_cat_3(.data$ecu_age)
    ) %>%
    select(matches(visitid), .data$ecu_age, .data$ecu_age_cat_dec, .data$ecu_age_cat_3)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  catw("done")

  return(trial_data)
}

#' Primary coding Body Mass Index (BMI)
#'
#' adds the following columns to fuv1: ecu_bmi, ecu_bmi_est, ecu_bmi_cat, ecu_bmi_est_cat
#'
#' @param trial_data SecuTrial Data
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
  catw("done")
  return(trial_data)
}

# Clinical Parameters ==========================================================

# the following clinical parameters are categorized according to primary coding 

# ============================================================================ #


#' Primary coding clinical parameters
#' 
#' adds the following variables to stv1: 
#' ecu_hpb, ecu_hf, ecu_oxy, ecu_temp, ecu_gcs, ecu_ph
#'
#' @param trial_data SecuTrial Data
#' @param pid column name of patient ID in trial_data
#' @param visitid column name of visit ID in trial_data
#' @importFrom rlang .data
#' @export
primary_coding_suep_clinical_params <- function(trial_data, pid, visitid) {

  formname_to_add_vars <- "stv1"
  
  form_to_add_vars <- trial_data[[formname_to_add_vars]]

  new_vars_to_add <- form_to_add_vars %>%
    mutate(ecu_hbp = categorize_bloodpressure_ecu(.data$gec_vitals_psys, .data$gec_vitals_pdias), 
           ecu_hf = categorize_heartfrequency_ecu(.data$gec_vitals_hf), 
           ecu_so2 = categorize_oxigensaturation_ecu(.data$gec_vitals_so2), 
           ecu_temp = categorize_temp_ecu(.data$gec_vitals_temp), 
           ecu_gcs = categorize_gcs_ecu(.data$vitals_gcs), 
           ecu_ph = categorize_ph_ecu(.data$gec_bga_ph)) %>%
    select(matches(visitid), .data$ecu_hbp, .data$ecu_hf, .data$ecu_so2, .data$ecu_temp, .data$ecu_gcs, .data$ecu_ph)
  
  trial_data[[formname_to_add_vars]] <- left_join(trial_data[[formname_to_add_vars]], new_vars_to_add, by=visitid)
  
  catw("done")
  return(trial_data)
}

# Scores =======================================================================

# the following scores  are categorized according to primary coding 

# ============================================================================ #



#' Primary coding Barthel index prior to infection (baseline)
#' 
#' adds the following variable to bv1:
#' ecu_bl_barthel_cat
#' 
#' uses helper function categorize_barthel_ecu 
#'
#' @param trial_data SecuTrial Data
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

  catw("done")
  return(trial_data)
}

#' Primary coding Barthel index (patient status)
#' 
#' adds the following variable to fuv1:
#' ecu_patient_barthel_cat
#' 
#' uses helper function categorize_barthel_ecu 
#'
#' @param trial_data SecuTrial Data
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
  
  catw("done")
  return(trial_data)
}

#' Primary coding MoCA
#' 
#' adds the following variable to fuv3:
#' ecu_moca_cat 
#'
#' @param trial_data SecuTrial Data
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
  
  catw("done")
  return(trial_data)
}


# SUEP Wrapper primary coding ==================================================

#' Primary coding SUEP Data
#' 
#' Wrapper function applying the following primary coding steps to trial_data: 
#' primary_coding_suep_age, primary_coding_suep_bmi, primary_coding_suep_clinical_params, 
#' primary_coding_suep_baseline_barthel, primary_coding_suep_patient_barthel, primary_coding_suep_moca
#'
#' @param trial_data Export from secuTrial
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

  # Demographics
  trial_data <- trial_data %>%
    primary_coding_suep_age(pid, visitid) %>%
    primary_coding_suep_bmi(pid, visitid) %>%

  # Clinical parameters
    primary_coding_suep_clinical_params(pid, visitid) %>%
  
  # Scores
    primary_coding_suep_baseline_barthel(pid, visitid) %>%
    primary_coding_suep_patient_barthel(pid, visitid) %>%
    primary_coding_suep_moca(pid, visitid)

  catw("Primary Coding done")
  
  return(trial_data)
}

