#################
# Primary coding functions developed for all NAPKON secutrial datasets
#
# script contains function definition which can be sourced for primary coding of secutrial datasets of all NAPKON platforms
#
# objective is to achieve a consistent and standardised preparation of variables: Categorization, definition of reference ranges, calculation of scores, etc. 
#################


#' Calculate full years from two dates
#'
#' @description calculates time difference in full years
#' @return time difference between two dates in full years as numeric value
#' @param from earlier date in yyyymmdd
#' @param to later date in yyyymmdd
#' @import lubridate 
#' @export 

calculate_full_years <- function(from, to){
  return(trunc((from %--% to) / years(1)))
  #return(floor(decimal_date(to) - decimal_date(from)))
}


#' Categorize age in decades 
#'
#' @description Categorize age in decades 

#' @param age vector of age in years
#' @return factor of age categories
#' @export

ecu_age_cat_dec <- function(age){
  cut(
    age, 
    breaks = c(-Inf, 17, 29, 39, 49, 59, 69, 79, Inf), 
    include.lowest = T, 
    labels = c("<18", "18-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
    ordered_result = TRUE
  )
}


#' Categorize age in three intervals
#'
#' @description  Categorize age in three intervals
#' @param age vector of age in years
#' @return factor of age categories
#' @export

ecu_age_cat_3 <- function(age){
  cut(
    age, 
    breaks = c(-Inf, 17, 59, 79, Inf), 
    include.lowest = T, 
    labels = c("<18", "18-59", "60-79", "80+"),
    ordered_result = TRUE
  )
}


#' Calculate time span in days
#'
#' @description  Calculate time span in days
#' @param start_form form which includes start date. NULL if one is not working across forms
#' @param startdate vector of start dates of time span or column name within start_form containing date variable
#' @param end_form form which includes end date. NULL if one is not working across forms
#' @param enddate vector of end dates of time span or column name within end_form containing date variable
#' @param primary_form "start" if the time span will be merged to start_form. "end" if the time span will be merged to end_form. NA if one is not working across forms
#' @param pid column name of patient ID in trial_data
#' @return vector of duration of time span in days
#' @note TODO: consider using start_form_mnppid and end_form_mnppid instead of entire forms, see hap/40-core-analysis
#' @importFrom rlang enquo
#' @importFrom lubridate interval
#' @import dplyr 
#' @export

calculate_days_ecu <- function(start_form = NULL, startdate, end_form = NULL, enddate, primary_form = NA, pid){
  
  # primary form not applicable: work with input vectors
  if(is.na(primary_form)){
    time_span <- time_length(interval(startdate, enddate), unit = "days")
  }
  
  # primary form == start: merge enddate to startdate and calculate time span
  if(!is.na(primary_form) & primary_form == "start"){
    startdate <- enquo(startdate)
    enddate <- enquo(enddate)
    
    time_span<- select(start_form, !!startdate, pid) %>%
      left_join(select(end_form, !!enddate, pid), by = pid) %>%
      mutate(time_span = time_length(interval(!!startdate, !!enddate), unit = "days")) %>%
      pull(time_span)
  }
  
  # primary form == end: merge startdate to enddate and calculate time span
  if(!is.na(primary_form) & primary_form == "end"){
    startdate <- enquo(startdate)
    enddate <- enquo(enddate)
    
    time_span<- select(end_form, !!enddate, pid) %>%
      left_join(select(start_form, !!startdate, pid), by = pid) %>%
      mutate(time_span = time_length(interval(!!startdate, !!enddate), unit = "days")) %>%
      pull(time_span)
  }
  
  return(time_span)
}


#' Compute body mass index (BMI)
#' 
# aus: https://rdrr.io/github/verasls/lvmisc/src/R/bmi.R
#TODO: how and whether to reference? 
#'
#' \code{bmi} calculates the BMI in kilograms per meter squared.
#'
#' @param mass,height A numerical vector with body mass and height data. `mass`
#'   unit must be kilograms and `height` unit must be meters. If the `height`
#'   unit is centimeters, it is converted to meters before BMI computation and
#'   a warning is shown.
#'
#' @return Returns a double vector with the element-wise body mass index (BMI).
#'
#' bmi(mass, height)
#' 
#' @export 

calculate_bmi <- function(mass, height) {
  #  if (!is.numeric(mass)) {
  #    abort_argument_type("mass", must = "be numeric", not = mass)
  #  }
  #  if (!is.numeric(height)) {
  #    abort_argument_type("height", must = "be numeric", not = height)
  #  }
  #  if (length(mass) != length(height)) {
  #    abort_argument_diff_length("mass", "height")
  #  }
  
  #   Deal with height in centimeters case, but converts the vector
  # TODO: rewrite to ifelse
  cm_thrsh <- 3
  mean_height <- mean(height, na.rm = TRUE)
  if (mean_height > cm_thrsh) {
    msg <- paste(
      "`height` unit is probably centimeters;",
      "converted to meters before computation."
    )
    rlang::warn(msg)
    height <- height / 100
  }
  
  #   Deal with weight in gram case
  #TODO: needs a) review and b) error message when mass is > 500
  mass <- ifelse (mass > 500, mass/1000, mass) 
  
  bmi <- mass / (height ^ 2)
  return(bmi)
}


#' Categorize BMI
#' 
#' @return factor of BMI categories
#'
#' @description Classify body mass index (BMI) category
#' bmi_cat(bmi)
#' @param bmi A numeric vector with BMI data. `BMI` unit must be meters per square meter.
#' @param height_uk.factor A factor indicating that height is unknown
#' @param weight_uk.factor A factor indicating that weight is unknown
#'
#' @importFrom forcats fct_expand
#' @export

categorize_bmi_ecu <- function(bmi, height_uk.factor=NULL, weight_uk.factor=NULL) {
  # if (!is.numeric(bmi)) {
  #  abort_argument_type("bmi", must = "be numeric", not = bmi)
  #}
  
  breaks_polytom <- c(1,18.5,25,30,35,40,Inf)
  labels_polytom <- c("Untergewicht", "Normalgewicht", "\u00fcbergewicht",
                      "Adipositas Grad I", "Adipositas Grad II", "Adipositas Grad III")
  
  
  bmi_cat <- cut(bmi, right = F, breaks = breaks_polytom, labels = labels_polytom)
  
  if(!is.null(height_uk.factor) & !is.null(weight_uk.factor)) {
  
  uk.factor_comb  <- if_else(!is.na(weight_uk.factor), weight_uk.factor, height_uk.factor)
  
  bmi_cat <- fct_expand(bmi_cat, levels(uk.factor_comb))
  
  
  bmi_cat <-  if_else(!is.na(bmi_cat),bmi_cat, uk.factor_comb)
  }
  
  return(bmi_cat)
}


#' Categorize barthel index
#'
#' @description Categorizes barthel index in four categories
#' @return factor of barthel index categories
#' @param barthel vector of Barthel index points
#' @export 

categorize_barthel_ecu <- function(barthel){
  factor(
    case_when(
      barthel >= 0 &  barthel <= 30 ~ "Weitgehend pflegeabh\u00e4ngig",
      barthel >= 35 &  barthel <= 80 ~ "Hilfsbed\u00fcrftig", 
      barthel >= 85 &  barthel <= 95 ~ "Punktuell hilfsbed\u00fcrftig",
      barthel >= 95 &  barthel <= 100 ~ "Selbstst\u00e4ndig"
    )
  )
}


#' Categorize Montreal Cognitive Assessment (MoCA)
#'
#' @description Categorizes MoCA-Score in two categories
#' @return factor of MoCA categories
#'
#' @param moca vector of MoCA points
#' @export 

categorize_moca_ecu <- function(moca){
  factor(
    case_when(moca >= 26 ~ "Normale kognitive Funktion", 
              moca < 26 ~ "Eingeschr\u00e4nkte kognitive Funktion")
  )
}


#' Calculate EQ5D-5L-Index
#' 
#' @description Calculates EQ-5D-5L index following recommendations for Germany
#' @param mo is a vector for mobility score of eq5d
#' @param sc is a vector for self-care score of eq5d
#' @param ua is a vector for usual activity score of eq5d
#' @param pd is a vector for pain/discomfort score of eq5d
#' @param ad is a vector for anxiety/depression score of eq5d
#' @importFrom eq5d eq5d
#' @return vector with eq5d-5l-index 
#' @export

calculate_eq5d5l_index <- function (mo, sc, ua, pd, ad) {
  ecu_eq5d_score <- paste (mo, sc, ua, pd, ad, sep="")
  ecu_eq5d5l_index <- eq5d(scores = ecu_eq5d_score, type = "VT", version = "5L", country = "Germany", ignore.invalid = TRUE)
  
  return (ecu_eq5d5l_index)
}


#' Categorize blood pressure
#' 
#' @description Categorizes blood pressure in seven categories
#' @return factor of blood pressure categories
#'
#' @param sbp vector of systolic blood pressure in mmHg 
#' @param dbp vector of diastolic blood pressure in mmHg 
#' @export 

categorize_bloodpressure_ecu <- function(sbp, dbp){
  factor(
    case_when(
      sbp >= 140 & dbp < 90 ~ "Isolated Systolic Hypertension",
      sbp < 120 & dbp < 80 ~ "Optimal", 
      sbp <= 129 & dbp <= 84 ~ "Normal", 
      sbp <= 139 & dbp <= 89 ~ "High Normal",
      sbp <= 159 & dbp <= 99 ~ "Hypertension Grade I", 
      sbp <= 179 & dbp <= 109 ~ "Hypertension Grade II", 
      sbp > 179 | dbp > 109 ~ "Hypertension Grade III")
  )
}


#' Categorize heartfrequency
#' 
#' @description Categorizes heartfrequency in three categories
#' @return factor of heartfrequency categories
#'
#' @param hf vector of heartfrequency in bpm
#' @export 

categorize_heartfrequency_ecu <- function(hf){
  factor(
    case_when(hf < 60 ~ "Bradykardie", 
              hf < 100 ~ "Physiologisch", 
              hf >= 100 ~ "Tachikardie")
  )
}


#' Categorize oxigen saturation
#'
#' @description Categorizes oxigen saturation in four categories
#' @return factor of oxigen saturation categories
#'
#' @param so2 vector of oxigen saturation SpO2 in percent
#' @export

categorize_oxigensaturation_ecu <- function(so2){
  factor(
    case_when(so2 < 85  ~ "Hochgradige Hypoxygenation", 
              so2 < 90  ~ "Mittelgradige Hypoxygenation",
              so2 < 95  ~ "M\u00e4ssige Hypoxygenation", 
              so2 <= 100  ~ "Normbereich")
  )
}


#' Categorize respiration rate
#' 
#' @description Categorizes respiration rate in three categories
#' @return factor of breathing categories
#'
#' @param resp_rate A vector with breaths per minute
#' @export

categorize_resp_rate_ecu <-  function(resp_rate){
  factor(
    case_when(
      resp_rate < 10 ~ "Bradypnoe", 
      resp_rate >= 10 & resp_rate <= 20 ~ "Normalbereich", 
      resp_rate > 20 ~ "Tachypnoe"))
}


#' Categorize body temperature
#' 
#' @description Categorizes body temperature in seven categories
#' @return factor of body temperature categories
#'
#' @param temp vector of body temperature in degree celsuis
#' @export 

categorize_temp_ecu <-  function(temp){
  factor(
    case_when(
      temp < 36.3 ~ "Erniedrigte Temperatur", 
      temp < 37.5 ~ "Normale Temperatur", 
      temp < 38.1 ~ "Erh\u00f6hte Temperatur",
      temp < 38.6 ~ "Leichtes Fieber",
      temp < 39.1 ~ "M\u00e4ssiges Fieber",
      temp < 40 ~ "Hohes Fieber",
      temp < 42.5 ~ "Sehr hohes Fieber"))
}


#' Categorize Glasgow Coma Scale
#' 
#' @description Categorizes GCS in four categories
#' @return factor of Glasgow Coma Scale categories
#'
#' @param gcs vector of Glasgow Coma Scale points
#' @export 

categorize_gcs_ecu <-  function(gcs){
  factor(
    case_when(
      gcs <= 8 ~ "Schwere Hirnsch\u00e4digung", 
      gcs <= 12 ~ "Mittelschwere Hirnsch\u00e4digung", 
      gcs <= 15 ~ "Leichte Hirnsch\u00e4digung", 
      gcs > 15 ~ "Keine Hirnsch\u00e4digung"))
}


#' Categorize Horowitz-Index (lung function)
#' 
#' @description Categorize Horowitz-Index in four categories
#' @return factor of Horowitz categories
#'
#' @param horowitz A vector with lung function measured in mmHg
#' @export

categorize_horowitz_index_ecu <-  function(horowitz){
  factor(
    case_when(
      horowitz > 300 ~ "Normal lung function ",
      horowitz > 200 & horowitz <= 300 ~ "Slightly impaired lung function",
      horowitz > 100 & horowitz <= 200 ~ "Moderately impaired lung function",
      horowitz <= 100 ~ "Severely impaired lung function"))
}


#' Categorize pH of blood gas analysis
#' 
#' @description Categorize blood gas analysis in three categories
#' @return factor of pH categories
#'
#' @param bga_ph vector of pH of blood gas analysis
#' @export

categorize_ph_ecu <-  function(bga_ph){
  factor(
      case_when(
        bga_ph < 7.35 ~ "Azidose", 
        bga_ph <= 7.45 ~ "Normbereich", 
        bga_ph > 7.45 ~ "Alkalose"))
}


#' Categorize modified Medical Research Council Dyspnea Scale (MMRC)
#' 
#' @description Categorize MMRC in dyspnea and no dispnea
#' @param mmrc factor of modified medical research council dyspnea scale (mmrc) categories
#' 
#' @return A factorized vector w/ levels "Dyspnea" and "No disypnea"

categorize_mmrc_ecu <- function (mmrc) {
  factor (
    case_when (mmrc == "Ich bekomme nur Atemnot bei sehr starker Belastung." ~ "No Dyspnea",
               mmrc != "Ich bekomme nur Atemnot bei sehr starker Belastung." ~ "Dyspnea")
  )
}


#' Calculate Patient Health Questionnaire Depression Scale 8 (PHQ 8) sum score
#' 
#' @description Calculate sum score of PHQ-8
#' 
#' Patients are asked "Over the last 2 weeks, how often have you been bothered by any of the following problems?"
#' @param phq8_1 vector for item "Little interest or pleasure in doing things."
#' @param phq8_2 vector for item "Feeling down, depressed, or hopeless."
#' @param phq8_3 vector for item "Trouble falling or staying asleep, or sleeping too much."
#' @param phq8_4 vector for item "Feeling tired or having little energy."
#' @param phq8_5 vector for item "Poor appetite or overeating"
#' @param phq8_6 vector for item "Feeling bad about yourself - or that you are a failure or have let yourself or your family down."
#' @param phq8_7 vector for item "Trouble concentrating on things, such as reading the newspaper or watching television."
#' @param phq8_8 vector for item "Moving or speaking so slowly that other people could have notived. Or the opposite - being so fidgety or restless that you have been moving around a lot more than usual."
#' 
#' #' Answers were coded /w levels 0 = "Not at all", 1 = "Several days", 2 = "More than half the days" and 3 = "Nearly every day"
#' 
#' @return A numeric vector with sum score of phq8
#' @export

calculate_phq8_sum <- function (phq8_1, phq8_2, phq8_3, phq8_4, phq8_5, phq8_6, phq8_7, phq8_8) {
  ecu_phq8_sum <- sum(phq8_1, phq8_2, phq8_3, phq8_4, phq8_5, phq8_6, phq8_7, phq8_8)
  
  return(ecu_phq8_sum)
}


#' Categorize Patient Health Questionnaire Depression Scale 8 (PHQ 8) 
#' 
#' @description Categorize PHQ-8 sum score in depression and no depression
#' @param ecu_phq8_sum numeric vector with sum score of phq8
#' @return A factorized vector w/ levels "No depression" and "Depression"
#' @export

categorize_phq8_ecu <- function (ecu_phq8_sum) {
  factor (
    case_when (ecu_phq8_sum < 10 ~ "No depression",
               ecu_phq8_sum >= 10 ~ "Depression")
  )
}


#' Categorize National Early Warning Score (NEWS)
#' 
#' @description Categorize NEWS in three categories
#' @param news_score A numerical vector with NEWS score
#' @return A factorized vector w/ levels "Small clinical risk", "Moderate Small clinical risk", "High clinical risk", 
#' @export

categorize_news_score_ecu <- function(news_score) {
  factor(
    case_when(news_score <= 4 ~ "Small clinical risk",
              news_score == 5 | news_score == 6 ~ "Moderate clinical risk",
              news_score >= 7 ~ "High clinical risk"))
}


#' Categorize Acute Physiology And Chronic Health Evaluation (APACHE) Score
#' 
#' @description Categorize APACHE Score in eight categories
#' @param apache_score A numerical vector with APACHE score
#' @return A factorized vector with risk to die during hospitalisation
#' @export

categorize_apache2_score_ecu <- function(apache_score) {
  factor(
    case_when(apache_score <= 4 ~ "~4%",
              apache_score >= 5 & apache_score <= 9 ~ "~8%",
              apache_score >= 10 & apache_score <= 14 ~ "~15%",
              apache_score >= 15 & apache_score <= 19 ~ "~25%",
              apache_score >= 20 & apache_score <= 24 ~ "~40%",
              apache_score >= 25 & apache_score <= 29 ~ "~55%",
              apache_score >= 30 & apache_score <= 34 ~ "~75%",
              apache_score >= 35 ~ "~85%")
  )
}


#' Categorize Intensive Care delir Screening Checklist (ICDSC)
#' 
#' @description Categorize ICDSC in three categories
#' @param icdsc_score A numerical vector with ICDSC score
#' @return A factor /w levels "No delir", "Subsyndromale delir" and "Delir"
#' @export

categorize_icdsc_score_ecu <- function(icdsc_score) {
  factor(
    case_when(icdsc_score == 0 ~ "No delir",
              icdsc_score >= 1 & icdsc_score <= 3 ~ "Subsyndromale delir",
              icdsc_score >= 4 ~ "Delir")
  )
}


#' Categorize delir Detection Score (DDS)
#' 
#' @description Categorize DDS in delir and no delir
#' @param dds_score A numerical vector with DDS score
#' @return A factor /w levels "No delir" and "Delir"
#' @export

categorize_dds_score_ecu <- function(dds_score) {
  factor(
    case_when(dds_score <= 7 ~ "No delir",
              dds_score > 7 ~ "Delir")
  )
}


#' Calculate Generalized Anxiety Disorder 7 (GAD-7) sum score
#' 
#' @description Calculate GAD-7 sum score
#' 
#' Patients are asked "Over the last 2 weeks, how often have you been bothered by the following problems?"
#' @param gad7_1 vector for item "Feeling nervous, anxious or on edge."
#' @param gad7_2 vector for item "Not being able to stop or control worrying."
#' @param gad7_3 vector for item "Worrying too much about different things."
#' @param gad7_4 vector for item "Trouble relaxing."
#' @param gad7_5 vector for item "Being so restless that it is hard to sit still."
#' @param gad7_6 vector for item "Becoming easily annoyed or irritable."
#' @param gad7_7 vector for item "Feeling afraid as if something awful might happen."
#' 
#' Answers were coded /w levels 0 = "Not at all", 1 = "Several days", 2 = "More than half the days" and 3 = "Nearly every day"
#' 
#' @return A numeric vector with sum score of GAD-7
#' @export

calculate_gad7_sum <- function (gad7_1, gad7_2, gad7_3, gad7_4, gad7_5, gad7_6, gad7_7){
  ecu_gad7_sum <- sum(gad7_1, gad7_2, gad7_3, gad7_4, gad7_5, gad7_6, gad7_7) 
  
  return(ecu_gad7_sum)
}


#' Categorize Generalized Anxiety Disorder 7 (GAD-7) sum score
#' 
#' @description Categorize GAD-7 sum score in four categories
#' @param ecu_gad7_sum numeric vector with sum score of GAD-7
#' @return A factorized vector w/ levels "No anxiety", "Mild anxiety", "Moderate anxiety" and "Severe anxiety"
#' @export

categorize_gad7_ecu <- function (ecu_gad7_sum) {
  factor (
    case_when (ecu_gad7_sum < 5 ~ "No anxiety",
               ecu_gad7_sum >= 5 & ecu_gad7_sum < 10 ~ "Mild anxiety",
               ecu_gad7_sum >= 10 & ecu_gad7_sum < 15 ~ "Moderate anxiety",
               ecu_gad7_sum >= 15 ~ "Severe anxiety")
  )
}


#' Calculate Functional Assessment of Chronic Illness Therapy - Fatigue (FACIT-F) sum score
#' 
#' @description Calculate FACIT-F sum score
#' 
#' Patients are asked "Please indicate how much each of the following statements has applied to you over the last 7 days by selecting the appropriate item."
#' @param facitf_1 vector for item "I feel fatigued."
#' @param facitf_2 vector for item "I feel weak all over."
#' @param facitf_3 vector for item "I feel listless ("washed out")."
#' @param facitf_4 vector for item "I feel tired."
#' @param facitf_5 vector for item "I have trouble starting things because I am tired."
#' @param facitf_6 vector for item "I have trouble finishing things because I am tired."
#' @param facitf_7 vector for item "I have energy."
#' @param facitf_8 vector for item "I am able to do my usual activities."
#' @param facitf_9 vector for item "I need to sleep during the day."
#' @param facitf_10 vector for item "I am too tired to eat."
#' @param facitf_11 vector for item "I need help doing my usual activities."
#' @param facitf_12 vector for item "I am frustrated by being too tired to do the things I want to do."
#' @param facitf_13 vector for item "I have to limit my social activity because I am tired."
#' 
#' Answers were coded /w levels "Not at all", "A little bit", "Somewhat", "Quite a bit" and "Very much"
#' 
#' @return A numeric vector with sum score of FACIT-F
#' @export
 
calculate_facitf_sum <- function(facitf_1,facitf_2, facitf_3, facitf_4, facitf_5, facitf_6, facitf_7, facitf_8, facitf_9, facitf_10,
                                 facitf_11, facitf_12, facitf_13) {
  ecu_facitf_sum <- sum(facitf_1,facitf_2, facitf_3, facitf_4, facitf_5, facitf_6, facitf_7, facitf_8, facitf_9, facitf_10,
                        facitf_11, facitf_12, facitf_13)
  
  return(ecu_facitf_sum)
}


#' Categorize Functional Assessment of Chronic Illness Therapy - Fatigue (FACIT-F)
#' 
#' @description Categorize FACIT-F sum score in no fatigue and relevant fatigue
#' @param ecu_facitf_sum numeric vector with sum score of FACIT-F
#' @return A factorized vector w/ levels "Relevant fatigue" and "No fatigue"
#' @export

categorize_facitf_ecu <- function (ecu_facitf_sum) {
  factor (
    case_when (ecu_facitf_sum < 30 ~ "Relevant fatigue",
               ecu_facitf_sum >= 30 ~ "No fatigue")
  )
}


#' Calculate Brief Resilience Scale (BRS) sum score
#' 
#' @description Calculate BRS sum score
#' 
#' Patients are asked "The following questions are about your resilience to cope with crises. Please respond to each item by marking one box per row."
#' @param brs_1 vector for item "I tend to bounce back quickly after hard times.."
#' @param brs_2 vector for item "I have a hard time making it through stressful events."
#' @param brs_3 vector for item "It does not take me long to recover from a stressful event."
#' @param brs_4 vector for item "It is hard for me to snap back when something bad happens."
#' @param brs_5 vector for item "I usually come through difficult times with little trouble."
#' @param brs_6 vector for item "I tend to take a long time to get over set-backs in my life."
#' 
#' Answers were coded /w levels "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"
#' 
#' @return A numeric vector with sum score of BRS
#' @export

calculate_brs_sum <- function(brs_1, brs_2, brs_3, brs_4, brs_5, brs_6) {
  ecu_brs_sum <- sum(ifelse(brs_1 >= 1, brs_1, NA), ifelse(brs_2 >= 1, brs_2, NA), ifelse(brs_3 >= 1, brs_3, NA), ifelse(brs_4 >= 1, brs_4, NA), 
                     ifelse(brs_5 >= 1, brs_5, NA), ifelse(brs_6 >= 1, brs_6, NA), na.rm = TRUE)
  ecu_brs_sum <- case_when(is.na(brs_1) & is.na(brs_2) & is.na(brs_3) & is.na(brs_4) & is.na(brs_5) & is.na(brs_6) ~ NA_integer_,
                           brs_1 == "-1" & brs_2 == "-1" & brs_3 == "-1" & brs_4 == "-1" & brs_5 == "-1" & brs_6 == "-1" ~ NA_integer_,
                           TRUE ~ ecu_brs_sum)
  
  return(ecu_brs_sum)
}


#' Calculate Brief Resilience Scale (BRS) number of answered questions
#' 
#' @description Count number of anserwed items of BRS
#' 
#' @param brs_1 vector for item "I tend to bounce back quickly after hard times.."
#' @param brs_2 vector for item "I have a hard time making it through stressful events."
#' @param brs_3 vector for item "It does not take me long to recover from a stressful event."
#' @param brs_4 vector for item "It is hard for me to snap back when something bad happens."
#' @param brs_5 vector for item "I usually come through difficult times with little trouble."
#' @param brs_6 vector for item "I tend to take a long time to get over set-backs in my life."
#' @return A numeric vector with number of answered questions of BRS
#' @export

calculate_brs_n <- function(brs_1, brs_2, brs_3, brs_4, brs_5, brs_6) {
  
  ecu_brs_n <- sum(ifelse(!is.na(brs_1), 1, 0),
                   ifelse(!is.na(brs_2), 1, 0),
                   ifelse(!is.na(brs_3), 1, 0),
                   ifelse(!is.na(brs_4), 1, 0),
                   ifelse(!is.na(brs_5), 1, 0),
                   ifelse(!is.na(brs_6), 1, 0))

  ecu_brs_n <- ifelse(ecu_brs_n == 0, NA_integer_, ecu_brs_n)
  
  return(ecu_brs_n)
}


#' Calculate Brief Resilience Scale (BRS) total score
#' 
#' @description Calculate BRS total score by dividing BRS sum score through number of answered items of BRS
#' @param ecu_brs_sum A numeric vector with sum score of BRS
#' @param ecu_brs_n A numeric vector with number of answered questions of BRS
#' 
#' @return A numeric vector with total score of BRS
#' @export

calculate_brs_total <- function(ecu_brs_sum, ecu_brs_n) {
  ecu_brs_total <- round(ecu_brs_sum/ecu_brs_n, digits = 2)
  
  return(ecu_brs_total)
}


#' Categorize Brief Resilience Scale (BRS)
#' 
#' @description Categorize BRS total score in three categories 
#' @param ecu_brs_total A numeric vector with total score of BRS
#' 
#' @return A factorized vector /w levels "Low resilience", "Normal resilience" and "High resilience"
#' @export

categorize_brs_ecu <- function(ecu_brs_total) {
  factor (
    case_when (ecu_brs_total >= 1 & ecu_brs_total <= 2.99 ~ "Low resilience",
               ecu_brs_total >= 3 & ecu_brs_total <= 4.30 ~ "Normal resilience",
               ecu_brs_total >= 4.31 & ecu_brs_total <= 5 ~ "High resilience")
  )
}

