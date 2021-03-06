% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_sofa_r.R
\name{gen_sofa_r}
\alias{gen_sofa_r}
\title{Generates the SOFA respiratory score}
\usage{
gen_sofa_r(dt, pf_, ppv_ = NULL)
}
\arguments{
\item{dt}{data.table containing physiology data}

\item{pf_}{PF ratio}

\item{ppv_}{logical vector for positive pressure ventilation}
}
\description{
Generates the SOFA Respiratory score; requires PF ratio and optionally
an indicator for postive pressure ventilation
}
\examples{
# gen_sofa_r(ddata, pf_ = pf_ratio, ppv_ = ppv)
# ddata[pf_ratio<1000/7.6][sample(nrow(ddata[pf_ratio<1000/7.6]),20), .(pf_ratio, Total_Resp_Rate_Ventil, sofa_r)]
# table(ddata$sofa_r, useNA="always")
}

