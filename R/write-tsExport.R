#' Write tsExport exports to other formats
#' @description Convert the export prepared in R and export it to
#'              SPSS (sav), Stata (dta) or SAS (sas, xpt version 8)
#'              using the haven package.
#' @name write_tsExport
#' @param object \code{tsExport} object
#' @param ... further parameters
#' @details Due to variable naming limitations in other packages, date variables are
#'          appended with _d (rather than _date), datetime/POSIX variables are appended
#'          with _dt (rather than _datetime) and factors with _cat (rather than _factor).
#'          Further variable names may be altered in the conversion process.
#'          For details please refer to the \code{haven} documentation.
#'          Since this has not been heavily tested or used there may be issues and you might prefer doing this manually
#'          with the haven package. One particular sticking point is the length of variable names - R is not restrictive in
#'          this respect, but other software can be. read_tsExport does not truncate names, prefering to leave this to the
#'          user, which can cause write_tsExport() to fail with an error.
#' @references
#' Conversion to SPSS, STATA, SAS
#' https://github.com/SwissClinicalTrialOrganisation/secuTrialR/blob/master/R/write_secuTrial.R
#' https://cran.r-project.org/web/packages/secuTrialR/vignettes/secuTrialR-package-vignette.pdf
#' @return a list of filenames
#' @export
#' @import haven
#' @examples
#' \dontrun{
#' # read ts data
#' EXPORT_DIR <- file.path("/path/to/transferstelle_export",
#'                         "export_directory")
#' # load all export data
#' tsExport <- read_tsExport(EXPORT_DIR, separator = ",", decimal = ".")
#' tdir <- tempdir()
#' write_tsExport(tsExport, format = "sav", path = tdir)
#' list.files(tdir)
#' }
#' 
#'
write_tsExport <- function(object, ...) UseMethod("write_tsExport", object)
#' @export
#' @name write_tsExport
#' @param format format in which to save the export (one of "dta", "sas", "sav", "xpt")
#' @param metadata if TRUE then metadate files will also be written
write_tsExport.tsExportdata <- function(object, format = "dta", metadata = FALSE, ...) {
  
  if (! format %in% c("dta", "sas", "sav", "xpt")) {
    stop(paste0("format must be one of 'dta', 'sas', 'sav', 'xpt'. You specified: ", format))
  }
  x <- object$export_options$data_names
  names(x) <- NULL
  if (!metadata) x <- x[!x %in% object$export_options$meta_names]
  
  lapply(x, function(obs) {
    tmp <- object[[obs]]
    write_tsExport(tmp, filename = obs, format = format, ...)
  })
}

#' @name write_tsExport
#' @param df a data.frame
#' @param filename file name
#' @param path directory where the files should be saved
write_tsExport.data.frame <- function(df, filename, path = "", format = "dta", ...) {
  name <- names(df)
  # if (format %in% c("dta", "sav")) {
  name <- gsub("\\.datetime", "_dt", name)
  name <- gsub("\\.date", "_d", name)
  name <- gsub("\\.factor", "_cat", name)
  # }
  names(df) <- name
  
 # df <- convertnames(df, format)
  format2 <- format
  if (format == "sas") format2 <- "sas7bdat"
  out <- file.path(path, paste0(filename, ".", format2))
  if (format == "dta") haven::write_dta(df, out, ...)
  if (format == "sav") haven::write_sav(df, out, ...)
  if (format == "sas") haven::write_sas(df, out, ...)
  if (format == "xpt") haven::write_xpt(df, out, version = 8, ...)
  paste("Saved to", out)
}


# convert names (used in write_ts_export)
# convertnames <- function(df, format) {
#   name <- names(df)
#   # if (format %in% c("dta", "sav")) {
#   name <- gsub("\\.datetime", "_dt", name)
#   name <- gsub("\\.date", "_d", name)
#   name <- gsub("\\.factor", "_cat", name)
#   # }
#   names(df) <- name
#   return(df)
# }
