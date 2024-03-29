% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_stats_binclass.R
\name{calc_stats_binclass}
\alias{calc_stats_binclass}
\title{calc_stats_binclass
returns sensitivity, specificity, positive predictive value, etc. from input true positive, true negative, false positive, false negative. from binary classification. https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Relationship}
\usage{
calc_stats_binclass(
  tp,
  fp,
  fn,
  tn,
  targets = c("actual.positive", "actual.negative", "predicted.positive",
    "predicted.negative", "sensitivity", "specificity", "false.positive.rate",
    "false.negative.rate", "positive.predictive.value", "negative.predictive.value",
    "false.omission.rate", "false.discovery.rate", "positive.likelihood.ratio",
    "negative.likelihood.ratio", "markedness", "diagnostic.odds.ratio",
    "total.populatio", "prevalence", "accuracy", "balanced accuracy", "informedness",
    "prevalence.threshold", "f1.score", "fowlkes-mallows.index", 
    
    "matthews.correction.coefficient", "threat.score")
)
}
\arguments{
\item{tp}{true positive number}

\item{fp}{false positive number (type 1 error)}

\item{fn}{false negative number (type 2 error)}

\item{tn}{true negative number}

\item{targets}{character vector of statistics to calculate}
}
\description{
calc_stats_binclass
returns sensitivity, specificity, positive predictive value, etc. from input true positive, true negative, false positive, false negative. from binary classification. https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Relationship
}
