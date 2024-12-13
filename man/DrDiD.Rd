% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DrDiD.R
\name{DrDiD}
\alias{DrDiD}
\title{Title}
\usage{
DrDiD(
  yname,
  dname,
  gname,
  tname,
  idname,
  data,
  exp_link = c("logit", "gaussian"),
  wname = NULL,
  xformla = ~0,
  FE_1 = ~0,
  FE_2 = ~0,
  B = 30
)
}
\arguments{
\item{yname}{Outcome variable}

\item{dname}{Treatment variable}

\item{gname}{Treatment cohort, the period a unit is first treated}

\item{tname}{Periods}

\item{idname}{Unit id}

\item{data}{Raw dataset}

\item{exp_link}{Link of the exposure/propensity model, can be e.g. "logit" or "identity"}

\item{wname}{Weights}

\item{xformla}{Covariates formula}

\item{FE_1}{Two-way fixed effects, e.g. time and region.}

\item{FE_2}{One-way fixed effects, e.g. region. Here, you cannot use same as "idname" and "tname" as
those are assigned to either treatment and control group (idname) or in some applications, do not occur
in post period (tname).}

\item{B}{Number of boostrap samples in order to calculate standard errors}
}
\value{
Estimated ATT with boostrap standard errors
}
\description{
Title
}
