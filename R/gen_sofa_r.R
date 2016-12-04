#' @title Generates the SOFA respiratory score
#'
#' @description
#' Generates the SOFA Respiratory score; requires PF ratio and optionally
#' an indicator for postive pressure ventilation
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param pf_ PF ratio
#' @param ppv_ logical vector for positive pressure ventilation
#'
#' @examples
#' # gen_sofa_r(ddata, pf_ = pf_ratio, ppv_ = ppv)
#' # ddata[pf_ratio<1000/7.6][sample(nrow(ddata[pf_ratio<1000/7.6]),20), .(pf_ratio, Total_Resp_Rate_Ventil, sofa_r)]
#' # table(ddata$sofa_r, useNA="always")

#' @export
gen_sofa_r <- function(dt, pf_, ppv_ = NULL) {
    #  ======================
    #  = SOFA - respiratory =
    #  ======================
    # - [ ] TODO(2016-05-20): # OPTIONALLY - works with S:F ratio

    # appending _ to var names for readability and to ensure uses scoped version
    # need to pre-calculate a LOCF of ventilation
    # requires pf_ AND ppv_ (positive pressure vent)


    # library(data.table)
    # data.table changes the object in place unless you use dt1 <- copy(dt)
    # so passing data.tables via function is actually just passing a reference

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    pf_ <- as.character(pars$pf_)
    if (!is.null(pars$ppv_)) ppv_ <- as.character(pars$ppv_)


    # Set to NA by default (numeric)
    dt[, `:=`(sofa_r = as.numeric(NA))]

    # Define conditions via dummy vars
    # Default ppv to FALSE unless not missing AND TRUE
    if (!is.null(pars$ppv_)) {
        dt[, ppv_0 := ifelse(!is.na(get(ppv_)), get(ppv_), FALSE)]
    } else {
        warning("*** No indicator variable for mechanical ventilation: sofa_r max <=2")
        dt[, ppv_0 := FALSE]
    }

    # Update based on conditions
    # Order of conditions is IMPORTANT

    # SOFA = 0
    dt[get(pf_) >= 400/7.5, "sofa_r" := 0]

    # SOFA = 1
    dt[get(pf_) < 400/7.5, "sofa_r" := 1]

    # SOFA = 2
    dt[get(pf_) < 300/7.5, "sofa_r" := 2]

    # SOFA = 3
    dt[get(pf_) < 200/7.5 & ppv_0, "sofa_r" := 3]

    # SOFA = 4
    dt[get(pf_) < 100/7.5 & ppv_0, "sofa_r" := 4]

    # Tidy up dummy variables
    dt[, ppv_0 := NULL]

}
