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

PredictAndDrawAll = function(DATA, ...) {
  allPredictions = matrix(c(0), nrow = 7, ncol=length(DATA[[2]][,"party_changed"]))
  expected = DATA[[2]][,"party_changed"]

  allPredictions[1,] = bayesPrediction(DATA[[1]], DATA[[2]])
  allPredictions[2,] = treePrediction(DATA[[1]], DATA[[2]])
  allPredictions[3,] = tresholdPrediction(DATA[[1]], DATA[[2]], "own_dst")
  allPredictions[4,] = tresholdPrediction(DATA[[1]], DATA[[2]], "own_dst_norm")
  allPredictions[5,] = tresholdPrediction(DATA[[1]], DATA[[2]], "own_mode_dst")
  allPredictions[6,] = tresholdPrediction(DATA[[1]], DATA[[2]], "avg_streak_against_party")
  allPredictions[7,] = tresholdPrediction(DATA[[1]], DATA[[2]], "max_streak_against_party")
  colors = c("red","green","blue","yellow","brown","black","orange")

  for (i in 1:7) {
    draw_roc_curve(allPredictions[i,], expected, add=(i>1), lwd=3, col=colors[i], ...)
  }

  legend(x=0.465, y=0.3,
         legend = c("Naive Bayes",
                    "Decisive Tree",
                    "Treshold - party vote dst.",
                    "Treshold - party vote dst. norm.",
                    "Treshold - party vote mode dst.",
                    "Treshold - avg. streak against party mode",
                    "Treshold - max. streak against party mode"),
         lwd = 3,
         col = colors)
  #draw_roc_curve(t(allPredictions), t(expected), colors=c("red","green","blue","yellow","brown","black","orange"))
}