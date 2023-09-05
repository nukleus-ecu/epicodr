#TODO: to put into a better place later. Belongs actually somewhere in the read function, but we have sT-Version and ts-Version
# 

#' @description sets the name of the participant ID depending on sT or ts export.
#' @return sT_or_ts_object with additional value in export options
#' @param sT_or_ts_object data obejct
#' @export 

set_id_names <- function(sT_or_ts_object) {
  
  
  data_names <- sT_or_ts_object$export_options$data_names
  
  meta_names <- sT_or_ts_object$export_options$meta_names
  
  studydata_names <- data_names %>% dplyr::setdiff(meta_names) 
  
  first_studydata_name <- studydata_names[1]

  
  first_studydata_form <- sT_or_ts_object[[first_studydata_name]]
  
  if(first_studydata_form %>% names %>% stringr::str_detect("mnppid") %>% any()){
    pid <- "mnppid" # Unique patient identifier associating an eCRF record to a patient record.
    docid <- "mnpdocid" # Each eCRF document record has a unique document identifier (parent document).
    subdocid <- "mnpdocid" # Unique identifier of the subdocument (in the e... tables), contains referenced mnpdocid
    visitid <- "mnpcvpid" # Unique identifier for each patient visit
    
    id_names <- data.frame(pid, docid, subdocid, visitid)
  }
  else if(first_studydata_form %>% names %>% stringr::str_detect("export_psn") %>% any()){
    pid <- "export_psn"
    visitid <- "mnpcvpid" 
    
    id_names <- data.frame(pid, visitid)
  }
  
  else if(first_studydata_form %>% names %>% stringr::str_detect("patientpsn") %>% any()){
    pid <- "patientpsn"
    visitid <- "mnpcvpid" 
    
    id_names <- data.frame(pid, visitid)
  }
  
  sT_or_ts_object$export_options$id_names <- id_names
  
  return(sT_or_ts_object)
}
