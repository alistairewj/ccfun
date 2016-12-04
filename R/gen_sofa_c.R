#' @title Generates the SOFA cardiovascular score
#'
#' @description
#' Generates the SOFA CVS score; requires MAP and norad and optionally
#' handles other vasopressors
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param map_ Mean Arterial Pressure
#' @param norad_ noradrenaline dose in mcg/kg/min
#'
#' @examples
#' # system.time(gen_sofa_c(ddata, map_=map, norad_=rx_norad, adr_=rx_adre, vasop_=rx_vasopr, dopa_=rx_dopa, dobu_=rx_dobu) )
#' # table(ddata$sofa_c, useNA="always")
#' # ddata[rx_adre<0.1][sample(nrow(ddata[rx_adre<0.1]),20), .(map, rx_norad, rx_adre, rx_dopa, rx_vasopr, rx_dobu, sofa_c)]


#' @export
gen_sofa_c <- function(dt, map_, norad_, adr_=NULL, dopa_=NULL, dobu_=NULL, vasop_=NULL) {
    #  =========================
    #  = SOFA - cardiovascular =
    #  =========================
    # appending _ to var names for readability and to ensure uses scoped version
    # requires map AND norad, other drugs optional

    # library(data.table)
    # data.table changes the object in place unless you use dt1 <- copy(dt)
    # so passing data.tables via function is actually just passing a reference

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    map_ <- as.character(pars$map_)
    norad_ <- as.character(pars$norad_)
    if (!is.null(pars$adr_)) adr_ <- as.character(pars$adr_)
    if (!is.null(pars$dopa_)) dopa_ <- as.character(pars$dopa_)
    if (!is.null(pars$dobu_)) dobu_ <- as.character(pars$dobu_)
    if (!is.null(pars$vasop_)) vasop_ <- as.character(pars$vasop_)


    # Set to NA by default (numeric)
    dt[, `:=`(sofa_c = as.numeric(NA))]

    # Define conditions via dummy vars

    # Update based on conditions
    # Order of conditions is IMPORTANT

    # SOFA = 0
    dt[get(map_) >= 70, "sofa_c" := 0]

    # SOFA = 1
    dt[get(map_) < 70, "sofa_c" := 1]

    # SOFA = 2
    if (!is.null(dobu_)) dt[get(dobu_) > 0 , "sofa_c" := 2]
    if (!is.null(dopa_)) dt[get(dopa_) > 0 & get(dopa_) <= 5, "sofa_c" := 2]

    # SOFA = 3
    dt[get(norad_) > 0 & get(norad_) <= 0.1, "sofa_c" := 3]
    if (!is.null(dopa_)) dt[get(dopa_) > 5 & get(dopa_) <= 15, "sofa_c" := 3]
    if (!is.null(adr_)) dt[get(adr_) > 0 & get(adr_) <= 0.1, "sofa_c" := 3]

    # SOFA = 4
    dt[get(norad_) > 0.1, "sofa_c" := 4]
    if (!is.null(adr_)) dt[get(adr_) > 0.1, "sofa_c" := 4]
    if (!is.null(dopa_)) dt[get(dopa_) > 15 , "sofa_c" := 4]
    if (!is.null(vasop_)) dt[get(vasop_) > 0, "sofa_c" := 4]

}
