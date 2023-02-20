#' Primary coding of secutrial datasets of all NAPKON platforms
#' Primary coding functions developed for all NAPKON secutrial datasets.
#'
#' objective is to achieve a consistent and standardised preparation of variables: Categorization, definition of reference ranges, calculation of scores, etc. 


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
    
    time_span<- select(start_form, !!startdate, start_form$mnppid) %>%
      left_join(select(end_form, !!enddate, end_form$mnppid), by = pid) %>%
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
  labels_polytom <- c("Untergewicht", "Normalgewicht", "Uebergewicht",
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
#' @return factor of barthel index categories
#' @param barthel vector of Barthel index points
#' @export 

categorize_barthel_ecu <- function(barthel){
  factor(
    case_when(
      barthel >= 0 &  barthel <= 30 ~ "Weitgehend pflegeabhaengig",
      barthel >= 35 &  barthel <= 80 ~ "Hilfsbeduerftig", 
      barthel >= 85 &  barthel <= 95 ~ "Punktuell hilfsbeduerftig",
      barthel >= 95 &  barthel <= 100 ~ "Selbststaendig"
    )
  )
}


#' Categorize Montreal Cognitive Assessment (MoCA)
#'
#' @return factor of MoCA categories
#'
#' @param moca vector of MoCA points
#' @export 

categorize_moca_ecu <- function(moca){
  factor(
    case_when(moca >= 26 ~ "Normale kognitive Funktion", 
              moca <26 ~ "Eingeschraenkte kognitive Funktion")
  )
}


#' Calculate EQ5D-5L-Index
#' 
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
#' @return factor of oxigen saturation categories
#'
#' @param so2 vector of oxigen saturation SpO2 in percent
#' @export

categorize_oxigensaturation_ecu <- function(so2){
  factor(
    case_when(so2 < 85  ~ "Hochgradige Hypoxygenation", 
              so2 < 90  ~ "Mittelgradige Hypoxygenation",
              so2 < 95  ~ "Maessige Hypoxygenation", 
              so2 <= 100  ~ "Normbereich")
  )
}


#' Categorize respiration rate
#'
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
#' @return factor of body temperature categories
#'
#' @param temp vector of body temperature in degree celsuis
#' @export 

categorize_temp_ecu <-  function(temp){
  factor(
    case_when(
      temp < 36.3 ~ "Erniedrigte Temperatur", 
      temp < 37.5 ~ "Normale Temperatur", 
      temp < 38.1 ~ "Erhoehte Temperatur",
      temp < 38.6 ~ "Leichtes Fieber",
      temp < 39.1 ~ "Maessiges Fieber",
      temp < 40 ~ "Hohes Fieber",
      temp < 42.5 ~ "Sehr hohes Fieber"))
}


#' Categorize Glasgow Coma Scale
#'
#' @return factor of Glasgow Coma Scale categories
#'
#' @param gcs vector of Glasgow Coma Scale points
#' @export 

categorize_gcs_ecu <-  function(gcs){
  factor(
    case_when(
      gcs <= 8 ~ "Schwere Hirnschaedigung", 
      gcs <= 12 ~ "Mittelschwere Hirnschaedigung", 
      gcs <= 15 ~ "Leichte Hirnschaedigung", 
      gcs > 15 ~ "Keine Hirnschaedigung"))
}


#' Categorize Horowitz-Index (lung function)
#'
#' @return factor of Horowitz categories
#'
#' @param horowitz A vector with lung function measured in mmHg
#' @export

categorize_horowitz_index_ecu <-  function(horowitz){
  factor(
    case_when(
      horowitz > 300 ~ "Normale Lungenfunktion ",
      horowitz > 200 & horowitz <= 300 ~ "Leicht eingeschränkte Lungenfunktion",
      horowitz > 100 & horowitz <= 200 ~ "Moderat eingeschränkte Lungenfunktion",
      horowitz <= 100 ~ "Stark eingeschränkte Lungenfunktion"))
}


#' Categorize pH of blood gas analysis
#'
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


#' Categorize modified Medical Research Council Dyspnea Scale
#' 
#' @param mmrc factor of modified medical research council dyspnea scale (mmrc) categories
#' @return A factorized vector w/ levels "dyspnea" and "no disypnea"

categorize_mmrc_ecu <- function (mmrc) {
  factor (
    case_when (mmrc == "Ich bekomme nur Atemnot bei sehr starker Belastung." ~ "Keine Dyspnoe",
               mmrc != "Ich bekomme nur Atemnot bei sehr starker Belastung." ~ "Dyspnoe")
  )
}


#' Calculate Patient Health Questionnaire Depression Scale 8 (PHQ 8) sum score
#' 
#' Patients are asked "Wie oft fühlten Sie sich im Verlauf der letzten 2 Wochen durch die folgenden Beschwerden beeinträchtigt?"
#' @param phq8_1 factor for item "Wenig Interesse oder Freude an Ihren Tätigkeiten."
#' @param phq8_2 factor for item "Niedergeschlagenheit, Schwermut oder Hoffnungslosigkeit."
#' @param phq8_3 factor for item "Schwierigkeiten, ein- oder durchzuschlafen, oder vermehrter Schlaf."
#' @param phq8_4 factor for item "Müdigkeit oder Gefühl, keine Energie zu haben."
#' @param phq8_5 factor for item "Verminderter Appetit oder übermäßiges Bedürfnis zu essen."
#' @param phq8_6 factor for item "Schlechte Meinung von sich selbst; Gefühl, ein Versager zu sein oder die Familie enttäuscht zu haben."
#' @param phq8_7 factor for item "Schwierigkeiten, sich auf etwas zu konzentrieren, z. B. beim Zeitung  lesen oder Fernsehen."
#' @param phq8_8 factor for item "Waren Ihre Bewegungen oder Ihre Sprache so verlangsamt, dass es auch anderen auffallen würde? Oder waren Sie im Gegenteil "zappelig" oder ruhelos und hatten dadurch einen stärkeren Bewegungsdrang als sonst?"
#' 
#' @return A numeric vector with sum score of phq8
#' @export

calculate_phq8_sum <- function (phq8_1, phq8_2, phq8_3, phq8_4, phq8_5, phq8_6, phq8_7, phq8_8) {
  ecu_phq8_sum <- sum(phq8_1, phq8_2, phq8_3, phq8_4, phq8_5, phq8_6, phq8_7, phq8_8)
  
  return(ecu_phq8_sum)
}


#' Categorize Patient Health Questionnaire Depression Scale 8 (PHQ 8) 
#' 
#' @param ecu_phq8_sum numeric vector with sum score of phq8
#' @return A factorized vector w/ levels "keine Depression" and "Depression"
#' @export

categorize_phq8_ecu <- function (ecu_phq8_sum) {
  factor (
    case_when (ecu_phq8_sum < 10 ~ "Keine Depression",
               ecu_phq8_sum >= 10 ~ "Depression")
  )
}


#' Categorize National Early Warning Score (NEWS)
#' 
#' @param news_score A numerical vector with NEWS score
#' @return A factorized vector w/ levels "Geringes klinisches Risiko", "Moderates Klinisches Risiko", "Hohes Klinisches Risiko", 
#' "Gering-Moderates klinisches Risiko (Wert 3 in irgendeiner der Kategorien)"
#' @export

categorize_news_score_ecu <- function(news_score) {
  factor(
    case_when(news_score <= 4 ~ "Geringes klinisches Risiko",
              news_score == 5 | news_score == 6 ~ "Moderates Klinisches Risiko",
              news_score >= 7 ~ "Hohes Klinisches Risiko"))
}


#' Categorize Acute Physiology And Chronic Health Evaluation (APACHE) Score
#' 
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


#' Categorize Intensive Care Delirium Screening Checklist (ICDSC)
#' 
#' @param icdsc_score A numerical vector with ICDSC score
#' @return A factor /w levels "Kein Delir", "V.a. subsyndromales Delir" and "Delir"
#' @export

categorize_icdsc_score_ecu <- function(icdsc_score) {
  factor(
    case_when(icdsc_score == 0 ~ "Kein Delir",
              icdsc_score >= 1 & icdsc_score <= 3 ~ "V.a. subsyndromales Delir",
              icdsc_score >= 4 ~ "Delir")
  )
}


#' Categorize Delirium Detection Score (DDS)
#' 
#' @param dds_score A numerical vector with DDS score
#' @return A factor /w levels "Kein Delir" and "Delir"
#' @export

categorize_dds_score_ecu <- function(dds_score) {
  factor(
    case_when(dds_score <= 7 ~ "Kein Delir",
              dds_score > 7 ~ "Delir")
  )
}


#' Calculate Generalized Anxiety Disorder 7 (GAD-7) sum score
#' 
#' Patients are asked "Wie oft fühlten Sie sich im Verlauf der letzten 2 Wochen durch die folgenden Beschwerden beeinträchtigt?"
#' @param gad7_1 factor for item "Nervosität, Ängstlichkeit oder Anspannung."
#' @param gad7_2 factor for item "Nicht in der Lage sein, Sorgen zu stoppen oder zu kontrollieren."
#' @param gad7_3 factor for item "Übermäßige Sorgen bezüglich verschiedener Angelegenheiten."
#' @param gad7_4 factor for item "Schwierigkeiten zu entspannen."
#' @param gad7_5 factor for item "Rastlosigkeit, so dass Stillsitzen schwer fällt."
#' @param gad7_6 factor for item "Schnelle Verärgerung oder Gereiztheit."
#' @param gad7_7 factor for item "Gefühl der Angst, so als würde etwas Schlimmes passieren."
#' 
#' Answers were coded /w levels 0 = "Überhaupt nicht", 1 = "An einzelnen Tagen", 2 = "An mehr als der Hälfte der Tage" and 3 = "Beinahme jeden Tag"
#' 
#' @return A numeric vector with sum score of GAD-7
#' @export

calculate_gad7_sum <- function (gad7_1, gad7_2, gad7_3, gad7_4, gad7_5, gad7_6, gad7_7){
  ecu_gad7_sum <- sum(gad7_1, gad7_2, gad7_3, gad7_4, gad7_5, gad7_6, gad7_7) 
  
  return(ecu_gad7_sum)
}


#' Categorize Generalized Anxiety Disorder 7 (GAD-7) sum score
#' 
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
#' Patients are asked "Bitte geben Sie jeweils an, wie sehr jede der folgenden Aussagen im Laufe der letzten 7 Tage auf Sie zugetroffen hat, 
#' indem Sie den entsprechenden Punkt auswählen."
#' @param facitf_1 factor for item "Ich bin erschöpft."
#' @param facitf_2 factor for item "Ich fühle mich insgesamt schwach."
#' @param facitf_3 factor for item "Ich fühle mich lustlos (ausgelaugt)."
#' @param facitf_4 factor for item "Ich bin müde."
#' @param facitf_5 factor for item "Es fällt mir schwer, etwas anzufangen, weil ich müde bin."
#' @param facitf_6 factor for item "Es fällt mir schwer, etwas zu Ende zu führen, weil ich müde bin."
#' @param facitf_7 factor for item "Ich habe Energie."
#' @param facitf_8 factor for item "Ich bin in der Lage, meinen gewohnten Aktivitäten nachzugehen (Beruf, Einkaufen, Schule, Freizeit, Sport usw.)."
#' @param facitf_9 factor for item "Ich habe das Bedürfnis, tagsüber zu schlafen."
#' @param facitf_10 factor for item "Ich bin zu müde, um zu essen."
#' @param facitf_11 factor for item "Ich brauche Hilfe bei meinen gewohnten Aktivitäten (Beruf, Einkaufen, Schule, Freizeit, Sport usw.)."
#' @param facitf_12 factor for item "Ich bin frustriert, weil ich zu müde bin, die Dinge zu tun, die ich machen möchte."
#' @param facitf_13 factor for item "Ich muss meine sozialen Aktivitäten einschränken, weil ich müde bin."
#' 
#' Answers were coded /w levels "Überhaupt nicht", "Ein Wenig", "Mäßig", "Ziemlich" and "Sehr"
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
#' Patients are asked "Bei den folgenden Fragen geht es um Ihre Widerstandsfähigkeit, Krisen zu bewältigen. 
#' Geben Sie bitte an, wie sehr Sie den folgenden Aussagen jeweils zustimmen."
#' @param brs_1 factor for item "Ich neige dazu, mich nach schwierigen Zeiten schnell zu erholen."
#' @param brs_2 factor for item "Es fällt mir schwer, stressige Situationen durchzustehen."
#' @param brs_3 factor for item "Ich brauche nicht viel Zeit, um mich von einem stressigen Ereignis zu erholen."
#' @param brs_4 factor for item "Es fällt mir schwer, zur Normalität zurückzukehren, wenn etwas Schlimmes passiert ist."
#' @param brs_5 factor for item "Normalerweise überstehe ich schwierige Zeiten ohne größere Probleme."
#' @param brs_6 factor for item "Ich brauche tendenziell lange, um über Rückschläge in meinem Leben hinwegzukommen."
#' 
#' Answers were coded /w levels "Stimme überhaupt nicht zu", "Stimme eher nicht zu", "Stimme eher zu", "Stimme vollkommen zu"
#' 
#' @return A numeric vector with sum score of BRS
#' @export

calculate_brs_sum <- function(brs_1, brs_2, brs_3, brs_4, brs_5, brs_6) {
  ecu_brs_sum <- sum(brs_1, brs_2, brs_3, brs_4, brs_5, brs_6, na.rm = TRUE)
  
  return(ecu_brs_sum)
}


#' Calculate Brief Resilience Scale (BRS) number of answered questions
#' 
#' @param brs_1 factor for item "Ich neige dazu, mich nach schwierigen Zeiten schnell zu erholen."
#' @param brs_2 factor for item "Es fällt mir schwer, stressige Situationen durchzustehen."
#' @param brs_3 factor for item "Ich brauche nicht viel Zeit, um mich von einem stressigen Ereignis zu erholen."
#' @param brs_4 factor for item "Es fällt mir schwer, zur Normalität zurückzukehren, wenn etwas Schlimmes passiert ist."
#' @param brs_5 factor for item "Normalerweise überstehe ich schwierige Zeiten ohne größere Probleme."
#' @param brs_6 factor for item "Ich brauche tendenziell lange, um über Rückschläge in meinem Leben hinwegzukommen."
#' @return A numeric vector with number of answered questions of BRS
#' @export

calculate_brs_n <- function(brs_1, brs_2, brs_3, brs_4, brs_5, brs_6) {
  
  ecu_brs_n <- 0 + 
    case_when(!is.na(brs_1) ~ 1, is.na(brs_1) ~ 0) +
    case_when(!is.na(brs_2) ~ 1, is.na(brs_2) ~ 0) +
    case_when(!is.na(brs_3) ~ 1, is.na(brs_3) ~ 0) +
    case_when(!is.na(brs_4) ~ 1, is.na(brs_4) ~ 0) +
    case_when(!is.na(brs_5) ~ 1, is.na(brs_5) ~ 0) +
    case_when(!is.na(brs_6) ~ 1, is.na(brs_6) ~ 0) 
  return(ecu_brs_n)
}


#' Calculate Brief Resilience Scale (BRS) total score
#' 
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

