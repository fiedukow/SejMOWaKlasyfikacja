library("party") ##library with decision tree
library("e1071") ##library with naive bayes

source("treshold_prediction.r")

bayesPrediction = function(data_build, data_testing) {
  bayes = 
    naiveBayes(as.factor(party_changed) ~ own_dst +
                                          own_mode_dst +
                                          own_dst_norm +
                                          avg_streak_against_party +
                                          max_streak_against_party,
               data = data_build)
  prediction = predict(bayes, newdata = data_testing, type="raw")
  prediction = prediction[(length(prediction)/2+1):length(prediction)]
  
  return(prediction)
}

treePrediction = function(data_build, data_testing) {
  decisionTree = ctree(party_changed ~ own_dst +
                                       own_mode_dst +
                                       own_dst_norm +
                                       avg_streak_against_party +
                                       max_streak_against_party,
                       data = data_build)
  predict(decisionTree, newdata = data_testing)
}

tresholdPrediction = function(data_build, data_testing, statistic) {
  data_testing[,statistic]
}