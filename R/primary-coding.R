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
#' @return vector of duration of time span in days
#' @note TODO: consider using start_form_mnppid and end_form_mnppid instead of entire forms, see hap/40-core-analysis
#' @importFrom rlang enquo
#' @importFrom lubridate interval
#' @import dplyr 
#' @export
calculate_days_ecu <- function(start_form = NULL, startdate, end_form = NULL, enddate, primary_form = NA){
  
  # primary form not applicable: work with input vectors
  if(is.na(primary_form)){
    time_span <- time_length(interval(startdate, enddate), unit = "days")
  }
  
  # primary form == start: merge enddate to startdate and calculate time span
  if(!is.na(primary_form) & primary_form == "start"){
    startdate <- enquo(startdate)
    enddate <- enquo(enddate)
    
    time_span<- select(start_form, !!startdate, start_form$mnppid) %>%
      left_join(select(end_form, !!enddate, end_form$mnppid), by = "mnppid") %>%
      mutate(time_span = time_length(interval(!!startdate, !!enddate), unit = "days")) %>%
      pull(time_span)
  }
  
  # primary form == end: merge startdate to enddate and calculate time span
  if(!is.na(primary_form) & primary_form == "end"){
    startdate <- enquo(startdate)
    enddate <- enquo(enddate)
    
    time_span<- select(end_form, !!enddate, mnppid) %>%
      left_join(select(start_form, !!startdate, mnppid), by = "mnppid") %>%
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
#' returns factor of heartfrequency categories
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
              so2 < 100  ~ "Normbereich")
  )
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