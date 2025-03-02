#' United States Deaths 2011-2020
#'
#' United States deaths, population estimates, and crude mortality rates for
#' ages 25+ from the CDC Multiple Causes of Death Files.
#'
#' @format A data frame with 140 rows and 6 columns.
#'
#' \describe{
#'   \item{gender}{Gender}
#'   \item{age_group}{Attained age groups}
#'   \item{year}{Calendar year}
#'   \item{deaths}{Number of deaths}
#'   \item{population}{Population estimate}
#'   \item{qx}{Crude mortality rate equal to `deaths / population`}
#' }
#'
#' @source
#' Centers for Disease Control and Prevention, National Center for Health
#' Statistics. National Vital Statistics System, Mortality 1999-2020 on CDC
#' WONDER Online Database, released in 2021. Data are from the Multiple Cause
#' of Death Files, 1999-2020, as compiled from data provided by the 57 vital
#' statistics jurisdictions through the Vital Statistics Cooperative Program.
#' Accessed at <https://wonder.cdc.gov/mcd-icd10.html> on Jan 15, 2024."
#' @md
"us_deaths"
