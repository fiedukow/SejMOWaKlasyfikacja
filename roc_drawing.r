library("ROCR") ##library with roc curve

################################
##Draw roc curve with rocr lib##
####################################################################################################
##  	predictions: vector,matrix which contains predictions
##		labels: vector, matrix which contains true class labels. Must dimensions like predictions
####################################################################################################
draw_roc_curve = function(roc_predictions, roc_labels, ...)
{
  predicted_model <- prediction(roc_predictions, roc_labels) ##create prediction model
  perf <- performance(predicted_model, measure = "tpr", x.measure = "fpr") 
  plot(perf, ...)
}