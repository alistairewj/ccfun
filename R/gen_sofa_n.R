#' @title Generates the SOFA neuro score
#'
#' @description
#' Generates the SOFA neuro score; requires GCS
#'
#' @import data.table
#' @param dt data.table containing physiology data
#' @param gcs_ GCS
#'
#' @examples
#' # gen_sofa_n(ddata, gcs_ = gcs)
#' # table(ddata$sofa_n, useNA="always")
#' # ddata[gcs<=15][sample(nrow(ddata[gcs<=15]),20), .(gcs, sofa_n)]

#' @export
gen_sofa_n <- function(dt, gcs_) {
    #  ==============
    #  = SOFA - GCS =
    #  ==============

    # appending _ to var names for readability and to ensure uses scoped version

    # library(data.table)
    # data.table changes the object in place unless you use dt1 <- copy(dt)
    # so passing data.tables via function is actually just passing a reference

    # Non-standard evaluation
    pars <- as.list(match.call()[-1])

    gcs_ <- as.character(pars$gcs_)

    # Set to NA by default (numeric)
    dt[, `:=`(sofa_n = as.numeric(NA))]

    # Update based on conditions
    # Order of conditions is IMPORTANT

    # SOFA = 0
    dt[get(gcs_) == 15, "sofa_n" := 0]

    # SOFA = 1
    dt[get(gcs_) <= 14, "sofa_n" := 1]

    # SOFA = 2
    dt[get(gcs_) <= 12, "sofa_n" := 2]

    # SOFA = 3
    dt[get(gcs_) <=  9, "sofa_n" := 3]

    # SOFA = 4
    dt[get(gcs_) <=  5, "sofa_n" := 4]

    # - [ ] TODO(2016-05-21):  Now set to NA if patient sedated
    # if (!is.null(rxsed)) {
    #     print(describe(rxsed))
    #     update <- ifelse(rxsed %in% c("True", "TRUE", TRUE), NA, sofa.n)
    #     describe(update)
    #     sofa.n <- ifelse(is.na(update), NA, update)
    #     print(describe(sofa.n))
    # }

}
