#' calc_stats_binclass
#' returns sensitivity, specificity, positive predictive value, etc. from input true positive, true negative, false positive, false negative. from binary classification. https://en.wikipedia.org/wiki/Positive_and_negative_predictive_values#Relationship
#'
#' @param tp true positive number
#' @param fp false positive number (type 1 error)
#' @param fn false negative number (type 2 error)
#' @param tn true negative number
#' @param targets character vector of statistics to calculate
#'
#' @export


calc_stats_binclass <- function(
  tp, fp, fn, tn,
  targets = c(
    "actual.positive", "actual.negative", "predicted.positive", "predicted.negative",
    "sensitivity", "specificity", "false.positive.rate", "false.negative.rate",
              "positive.predictive.value", "negative.predictive.value", "false.omission.rate", "false.discovery.rate",
              "positive.likelihood.ratio", "negative.likelihood.ratio", "markedness", "diagnostic.odds.ratio",
              "total.populatio", "prevalence", "accuracy", "balanced accuracy", "informedness", "prevalence.threshold",
              "f1.score", "fowlkes-mallows.index", "matthews.correction.coefficient", "threat.score")
  ){


  output_stats <- vector("numeric", length = length(targets))
  names(output_stats) <- targets

  for(i in 1:length(output_stats)){

    this_target <- names(output_stats)[i]
     if(this_target == "actual.positive"){
      output_stats[i] <- actual.positive <- tp + fn
    } else if(this_target == "actual.negative"){
      output_stats[i] <- actual.negative <- fp + tn
    } else if(this_target == "predicted.positive"){
      output_stats[i] <- predicted.positive <- tp + fp
    } else if(this_target == "predicted.negative"){
      output_stats[i] <- predicted.negative <- fn + tn
    } else if(this_target == ""){
      output_stats[i] <- NA

    } else if(this_target == "sensitivity"){
      output_stats[i] <- sensitivity <- tp/actual.positive
    } else if(this_target == "specificity"){
      output_stats[i] <- specificity <- tn/actual.negative
    } else if(this_target == "false.positive.rate"){
      output_stats[i] <- false.positive.rate <- fp/actual.negative
    } else if(this_target == "false.negative.rate"){
      output_stats[i] <- false.negative.rate <- fn/actual.positive

    } else if(this_target == "positive.predictive.value"){
      output_stats[i] <- tp/predicted.positive
    } else if(this_target == "negative.predictive.value"){
      output_stats[i] <- tp/predicted.negative
    } else if(this_target == "false.omission.rate"){
      output_stats[i] <- fn/predicted.negative
    } else if(this_target == "false.discovery.rate"){
      output_stats[i] <- fp/predicted.positive

    } else if(this_target == "positive.likelihood.ratio"){
      output_stats[i] <- sensitivity/false.positive.rate
    } else if(this_target == "negative.likelihood.ratio"){
      output_stats[i] <- false.negative.rate/specificity
    } else if(this_target == ""){
      output_stats[i] <- NA
    } else{
      output_stats[i] <- NA
    }



  }

  output_stats


}