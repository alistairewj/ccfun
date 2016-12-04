#' @title Generates the SOFA renal score
#'
#' @description
#' Generates the SOFA renal score; requires creatinine and optionally urine
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param creat_ creatinine (micromol/l)
#' @param uvol24h_ 24 hourly urine volume
#' @param rrt_ current use of renal replacement therapy
#'
#' @examples
#' # gen_sofa_k(ddata, creat_ = creatinine)
#' # table(ddata$sofa_k, useNA="always")
#' # ddata[creatinine < 500][sample(nrow(ddata[creatinine < 500]),20), .(creatinine, sofa_k)]

#' @export
gen_sofa_k <- function(dt, creat_, uvol24h_ = NULL) {
    #  ================
    #  = SOFA - Renal =
    #  ================

    # - [ ] TODO(2016-05-20): add in RRT use as criteria
    # - [ ] TODO(2016-05-20): uvol24h_ will need interpolating and summarising from hourly

    # appending _ to var names for readability and to ensure uses scoped version

    # library(data.table)
    # data.table changes the object in place unless you use dt1 <- copy(dt)
    # so passing data.tables via function is actually just passing a reference

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    creat_ <- as.character(pars$creat_)
    # Will use 24 hour urine volumes if available
    if (!is.null(pars$uvol24h_)) uvol24h_ <- as.character(pars$uvol24h_)

    # Set to NA by default (numeric)
    dt[, `:=`(sofa_k = as.numeric(NA))]

    # Update based on conditions
    # Order of conditions is IMPORTANT

    # SOFA = 0
    dt[get(creat_) <  110, "sofa_k" := 0]

    # SOFA = 1
    dt[get(creat_) >= 110, "sofa_k" := 1]

    # SOFA = 2
    dt[get(creat_) >= 171, "sofa_k" := 2]

    # SOFA = 3
    dt[get(creat_) >= 300, "sofa_k" := 3]
    if (!is.null(uvol24h_)) dt[get(uvol24h_) < 500, "sofa_k" := 3]

    # SOFA = 4
    dt[get(creat_) >  440, "sofa_k" := 4]
    if (!is.null(uvol24h_)) dt[get(uvol24h_) < 200, "sofa_k" := 4]

}
