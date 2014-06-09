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

treePredictionRandomForestVariableSelection = function(data_build, data_testing) {
  decisionTree = ctree(party_changed ~ own_dst +
                                       own_mode_dst +
                                       own_dst_norm +
                                       avg_streak_against_party +
                                       max_streak_against_party,
                       data = data_build, control = ctree_control(mtry = 1))
  predict(decisionTree, newdata = data_testing)
}

treePredictionMoreSplit = function(data_build, data_testing) {
  decisionTree = ctree(party_changed ~ own_dst +
                                       own_mode_dst +
                                       own_dst_norm +
                                       avg_streak_against_party +
                                       max_streak_against_party,
                       data = data_build, control = ctree_control(minsplit = 5, minbucket = 2))
  predict(decisionTree, newdata = data_testing)
}

treePredictionLessThresholdToSplit = function(data_build, data_testing) {
  decisionTree = ctree(party_changed ~ own_dst +
                                       own_mode_dst +
                                       own_dst_norm +
                                       avg_streak_against_party +
                                       max_streak_against_party,
                       data = data_build, control = ctree_control(mincriterion = 0.2, testtype = "Univariate", mtry=1))
  predict(decisionTree, newdata = data_testing)
}

tresholdPrediction = function(data_build, data_testing, statistic) {
  data_testing[,statistic]
}

PredictAndDrawAll = function(DATA, filename = "tmp", ...) {
  allPredictions = matrix(c(0), nrow = 10, ncol=length(DATA[[2]][,"party_changed"]))
  expected = DATA[[2]][,"party_changed"]

  allPredictions[1,] = bayesPrediction(DATA[[1]], DATA[[2]])
  allPredictions[2,] = treePrediction(DATA[[1]], DATA[[2]])
  allPredictions[3,] = treePredictionRandomForestVariableSelection(DATA[[1]], DATA[[2]])
  allPredictions[4,] = treePredictionMoreSplit(DATA[[1]], DATA[[2]])
  allPredictions[5,] = treePredictionLessThresholdToSplit(DATA[[1]], DATA[[2]])
  allPredictions[6,] = tresholdPrediction(DATA[[1]], DATA[[2]], "own_dst")
  allPredictions[7,] = tresholdPrediction(DATA[[1]], DATA[[2]], "own_dst_norm")
  allPredictions[8,] = tresholdPrediction(DATA[[1]], DATA[[2]], "own_mode_dst")
  allPredictions[9,] = tresholdPrediction(DATA[[1]], DATA[[2]], "avg_streak_against_party")
  allPredictions[10,] = tresholdPrediction(DATA[[1]], DATA[[2]], "max_streak_against_party")
  colors = c("red","green","blue","yellow","brown","black","orange","pink","gray","darkgreen")

  png(filename = paste("./plots/", filename, ".png", sep=""),
      width = 640, height = 480, units = "px", pointsize = 12,
      bg = "white")
  for (i in 1:10) {
    draw_roc_curve(allPredictions[i,], expected, add=(i>1), lwd=3, col=colors[i], ...)
  }

  legend(x=0.465, y=0.33,
         legend = c("Naive Bayes",
                    "Decisive Tree",
                    "Decisive Tree - random forest variable selction",
                    "Decision Tree - more split",
                    "Decision Tree - less threshold to split",
                    "Treshold - party vote dst.",
                    "Treshold - party vote dst. norm.",
                    "Treshold - party vote mode dst.",
                    "Treshold - avg. streak against party mode",
                    "Treshold - max. streak against party mode"),
         lwd = 3,
         col = colors)

  dev.off();
}
