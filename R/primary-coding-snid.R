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
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    trial_data <- set_id_names(trial_data)
  }
  
  if (!("id_names" %in% names(trial_data$export_options))) {
    stop("No table named \"id_names\" in exportoptions. Did you use set_id_names()?")
  }
  
  pid <- trial_data$export_options$id_names$pid
  
  trial_data[[grep("^_?patinf$", table_names)]] <- trial_data[[grep("^_?patinf$", table_names)]] %>% 
    # get date of signed informed consent
    left_join(trial_data[[grep("^_?ic$", table_names)]] %>% select(all_of(pid),.data$ic_infcons_date.date), by = pid) %>%
    # birth date was only reported as year (YYYY), but needed to be YYYY-MM-DD --> we added -06-30 to set the date of birth to June 30th as middle of the respective year
    mutate(ecu_patinf_birthyear_new = ymd(paste0(.data$patinf_birthyear, "-06-30")),
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
    mutate(ecu_bmi = calculate_bmi(.data$patinf_weight, .data$patinf_height),
           ecu_bmi_cat = categorize_bmi_ecu(.data$ecu_bmi),
           ecu_bmi_adipositas = case_when(!is.na(.data$ecu_bmi_cat) ~  forcats::fct_collapse(.data$ecu_bmi_cat,
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
    mutate(ecu_hf = categorize_heartfrequency_ecu(.data$vital_freq))
  
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
    mutate(ecu_temp = categorize_temp_ecu(.data$vital_temp))
  
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
    mutate(ecu_resp_rate = categorize_resp_rate_ecu(.data$vital_resp))
  
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
    mutate(ecu_oxy_sat = categorize_oxigensaturation_ecu(.data$vital_spo2))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_oxy_sat = ""
  )
  
  return (trial_data)
}

# Scales =======================================================================

## Glasgow Coma Scale ==========================================================

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
    mutate(ecu_gcs = categorize_gcs_ecu(.data$vital_gcs))
  
  labelled::var_label(trial_data[[grep("^_?vital$", table_names)]]) <- list(
    ecu_gcs = ""
  )
  
  return (trial_data)
}

## Modified Rankin Scale (mRS) =================================================

#' Primary Coding Modified Rankin Scale
#' 
#' adds the following column to scorecns:
#' ecu_mrs_label
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_mrs <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?scorecns$", table_names)]] <-  trial_data[[grep("^_?scorecns$", table_names)]] %>%
    dplyr::mutate(ecu_mrs_label = get_labels_mrs(.data$scorecns_mrs_score))
  
  labelled::var_label( trial_data[[grep("^_?scorecns$", table_names)]]) <- list(
    ecu_mrs_label = ""
  )
  
  return(trial_data)
  
}


# Scores =======================================================================

# the following scores are categorized according to primary coding:
# EQ5D-5L, Meningitis Severity Score (MSS)

## EQ5D ========================================================================


#' Primary coding EQ5D-5L-Index
#' 
#' adds the following column to eq5d5l: 
#' ecu_eq5d_index
#'
#' @param trial_data A secuTrial data object
#' @import eq5d
#' @importFrom rlang .data
#' @export

primary_coding_snid_eq5d5l <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?eq5d5l$", table_names)]] <-  trial_data[[grep("^_?eq5d5l$", table_names)]] %>%
    mutate(ecu_eq5d5l_index = calculate_eq5d5l_index(.data$eq5d5l_mob, .data$eq5d5l_care, .data$eq5d5l_act, .data$eq5d5l_pain, .data$eq5d5l_anx))
  
  labelled::var_label( trial_data[[grep("^_?eq5d5l$", table_names)]]) <- list(
    ecu_eq5d5l_index = ""
  )
  
  return(trial_data)
  
}

## Meningitis Severity Score (MSS) =============================================


#' Primary coding Meningitis Severity Score (MSS)
#' 
#' adds the following column to scorecns: 
#' ecu_mss_label
#'
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_mss <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  trial_data[[grep("^_?scorecns$", table_names)]] <-  trial_data[[grep("^_?scorecns$", table_names)]] %>%
    mutate(ecu_mss_label = get_labels_mss(.data$scorecns_mss_score))
  
  labelled::var_label( trial_data[[grep("^_?scorecns$", table_names)]]) <- list(
    ecu_mss_label = ""
  )
  
  return(trial_data)
  
}


# Laboratory units =============================================================

# create common units for each laboratory measure

#' Primary coding laboratory units
#' 
#' add the following colums to lab:
#' ecu_lab_leuko_1E9pliter, ecu_lab_neutroph_1E9pliter, ecu_lab_lympho_1E9pliter, ecu_lab_crp_mgpliter,  
#' ecu_lab_pct_ngpml, ecu_lab_il6_pgpml, ecu_lab_ast_Upl, ecu_lab_crea_mgpdl, ecu_lab_lactate_mmolpl
#' 
#' @param trial_data A secuTrial data object
#' @importFrom rlang .data
#' @import dplyr
#' @export

primary_coding_snid_lab <- function(trial_data) {
  
  table_names <- names(trial_data)
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_leuko_1E9pliter = case_when(lab_leuko_u == 1 ~ lab_leuko / 1000, # lab_leuko_u == 1 = /mikroliter, needs recalculation
                                               lab_leuko_u == 2 ~ lab_leuko, # lab_leuko_u == 2 = /nanoliter, same as 10^9/liter
                                               lab_leuko_u == 3 ~ lab_leuko, # lab_leuko_u == 3 = x10^3/mikroliter, same as 10^9/liter
                                               lab_leuko_u == 4 ~ lab_leuko / 1000, # lab_leuko_u == 4 = Ts/mikroliter, needs recalculation
                                               lab_leuko_u == 5 ~ lab_leuko, # lab_leuko_u == 5 = 1E9/liter = 10^9/liter, most common unit, reference unit 
                                               lab_leuko_u == 6 ~ lab_leuko, # lab_leuko_u == 6 = Gpt/liter, same as 10^9/liter
                                               lab_leuko_u == 7 ~ lab_leuko))}, #lab_leuko_u == 7 = K/mikroliter, same as 10^9/liter
    error = function(e) {
      warning("Leucocyte unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_neutroph_1E9pliter = case_when(lab_neutroph_u == 1 ~ ecu_lab_leuko_1E9pliter * (lab_neutroph / 100), # lab_neutroph_u == 1 = %, needs recalculation
                                                  lab_neutroph_u == 2 ~ lab_neutroph / 1000, # lab_neutroph_u == 1 = /mikroliter, needs recalculation
                                                  lab_neutroph_u == 3 ~ lab_neutroph, # lab_neutroph_u == 3 = /nanoliter, same as 10^9/liter
                                                  lab_neutroph_u == 4 ~ lab_neutroph, # lab_neutroph_u == 4 = x10^3/mikroliter, same as 10^9/liter
                                                  lab_neutroph_u == 5 ~ lab_neutroph / 1000, # lab_neutroph_u == 5 = Ts/mikroliter, needs recalculation,
                                                  lab_neutroph_u == 6 ~ lab_neutroph, # lab_neutroph_u == 6 = 1E9/liter = 10^9/liter, most common unit, reference unit 
                                                  lab_neutroph_u == 7 ~ lab_neutroph))}, #lab_neutroph_u == 7 = Gpt/mikroliter, same as 10^9/liter
    error = function(e) {
      warning("Neutrophil unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_lympho_1E9pliter = case_when(lab_lympho_u == 1 ~ ecu_lab_leuko_1E9pliter * (lab_lympho / 100), # lab_lympho_u == 1 = %, needs recalculation
                                                lab_lympho_u == 2 ~ lab_lympho / 1000, # lab_lympho_u == 1 = /mikroliter, needs recalculation
                                                lab_lympho_u == 3 ~ lab_lympho, # lab_lympho_u == 3 = /nanoliter, same as 10^9/liter
                                                lab_lympho_u == 4 ~ lab_lympho, # lab_lympho_u == 4 = x10^3/mikroliter, same as 10^9/liter
                                                lab_lympho_u == 5 ~ lab_lympho / 1000, # lab_lympho_u == 5 = Ts/mikroliter, needs recalculation,
                                                lab_lympho_u == 6 ~ lab_lympho, # lab_lympho_u == 6 = 1E9/liter = 10^9/liter, most common unit, reference unit 
                                                lab_lympho_u == 7 ~ lab_lympho))}, #lab_lympho_u == 7 = Gpt/mikroliter, same as 10^9/liter
    error = function(e) {
      warning("Lymphocyte unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_crp_mgpliter = case_when(lab_crp_u == 1 ~ lab_crp,
                                            lab_crp_u == 2 ~ lab_crp * 10,
                                            lab_crp_u == 3 ~ lab_crp * 0.115))}, #*0.115, da mg/l = nmol/l * (Molare Masse / 10^6) und Molare Masse CRP = 115000 g/mol
    error = function(e) {
      warning("CRP unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_pct_ngpml = case_when(lab_pct_u == 1 ~ lab_pct, 
                                         lab_pct_u == 2 ~ lab_pct))}, #lab_pct_u == 2 = mikrogramm/liter, same as nanogram/ml
    error = function(e) {
      warning("PCT unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_il6_pgpml = case_when(lab_il6_u == 1 ~ lab_il6, # lab_il6_u == 1 = nanogram/liter, same as pg/ml
                                         lab_il6_u == 2 ~ lab_il6))}, 
    error = function(e) {
      warning("IL6 unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_ast_Upl = case_when(lab_ast_u == 1 ~ lab_ast * 60, # lab_ast_u == 1 = mikrokat/l, needs recalculation
                                       lab_ast_u == 2 ~ lab_ast, 
                                       lab_ast_u == 3 ~ lab_ast * 60))}, #lab_ast_u == 3 = mikromol/s/liter, needs recalculation
    error = function(e) {
      warning("AST unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_crea_mgpdl = case_when(lab_crea_u == 1 ~ lab_crea * 0.011312, # *0.011312, da mg/dl = mikromol/l * (Molare Masse / 1000) und Molare Masse Kreatinin = 113.12 g/mol
                                          lab_crea_u == 2 ~ lab_crea * 11.312, # *11,312, da mg/dl = nmol/ml * (Molare Masse / 10) und Molare Masse Kreatinin = 113.12 g/mol 
                                          lab_crea_u == 3 ~ lab_crea))},
    error = function(e) {
      warning("Creatinine unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  tryCatch(expr = {trial_data[[grep("^_?lab$", table_names)]] <-  trial_data[[grep("^_?lab$", table_names)]] %>%
    mutate(ecu_lab_lactate_mmolpl = case_when(lab_lactate_u == 1 ~ lab_lactate,
                                              lab_lactate_u == 2 ~ lab_lactate * 0.111))}, #*0.111, da mmol/l = (mg/dl * 10) / Molare Masse und Molare Masse Laktate = 90.08 g/mol
    error = function(e) {
      warning("Lactate unit could not be recalculated. This is likely due to missing variables.")
      print(e)})
  
  labelled::var_label( trial_data[[grep("^_?lab$", table_names)]]) <- list(
    ecu_lab_leuko_1E9pliter = "",
    ecu_lab_neutroph_1E9pliter = "", 
    ecu_lab_lympho_1E9pliter = "", 
    ecu_lab_crp_mgpliter = "", 
    ecu_lab_pct_ngpml = "", 
    ecu_lab_il6_pgpml = "", 
    ecu_lab_ast_Upl = "", 
    ecu_lab_crea_mgpdl = "",
    ecu_lab_lactate_mmolpl = ""
  )
  
  return (trial_data)
  
}


# SNID Wrapper primary coding ==================================================

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
  
  ## BMI =======================================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_bmi(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_bmi() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Heart frequency ===========================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_hf(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_hf() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Temperature ===============================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_temp(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_temp() did not work. This is likely due to missing variables.")
             print(e)})
  
  ## Respiration rate ==========================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_resp_rate(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_resp_rate() did not work. This is likely due to missing variables.")
             print(e)})
  ## Oxygen saturation ========================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_oxy_sat(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_oxy_sat() did not work. This is likely due to missing variables.")
             print(e)})
  ##Scale ======================================================================
  ## Glasgow Coma Scale ========================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_gcs(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_gcs() did not work. This is likely due to missing variables.")
             print(e)})
  ## Modified Rankin Scale =====================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_mrs(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_mrs() did not work. This is likely due to missing variables.")
             print(e)})
  ##Scores =====================================================================
  ### EQ-5D-5L =================================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_eq5d5l(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_eq5d5l() did not work. This is likely due to missing variables.")
             print(e)})
  ### MSS ======================================================================
  tryCatch(expr = {trial_data <- primary_coding_snid_mss(trial_data)},
           error = function(e) {
             warning("primary_coding_snid_mss() did not work. This is likely due to missing variables.")
             print(e)})
  
  
  catw("Primary Coding done")
  
  return(trial_data)
}
