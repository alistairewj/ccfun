#' @title Generates the SOFA liver score
#'
#' @description
#' Generates the SOFA liver score; requires bilirubin
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param bili_ bilirubin
#'
#' @examples
#' # gen_sofa_l(ddata, bilirubin_ = bilirubin)
#' # table(ddata$sofa_l, useNA="always")
#' # ddata[bilirubin>100][sample(nrow(ddata[bilirubin>100]),20), .(bilirubin, sofa_l)]

#' @export
gen_sofa_l <- function(dt, bili_) {
    #  ================
    #  = SOFA - liver =
    #  ================

    # appending _ to var names for readability and to ensure uses scoped version

    # library(data.table)
    # data.table changes the object in place unless you use dt1 <- copy(dt)
    # so passing data.tables via function is actually just passing a reference

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    bili_ <- as.character(pars$bili_)

    # Set to NA by default (numeric)
    dt[, `:=`(sofa_l = as.numeric(NA))]

    # Update based on conditions
    # Order of conditions is IMPORTANT

    # SOFA = 0
    dt[get(bili_) < 20, "sofa_l" := 0]

    # SOFA = 1
    dt[get(bili_) >= 20, "sofa_l" := 1]

    # SOFA = 2
    dt[get(bili_) >= 33, "sofa_l" := 2]

    # SOFA = 3
    dt[get(bili_) >= 102, "sofa_l" := 3]

    # SOFA = 4
    dt[get(bili_) >= 204, "sofa_l" := 4]

}
