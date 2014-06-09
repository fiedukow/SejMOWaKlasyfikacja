source('load_and_preprocess.r')
source('prediction_models.r')
source("roc_drawing.r")

#### BUILDING THE MODEL
DATA_50 = loadAndPreprocess(50);
DATA_100 = loadAndPreprocess(100);
DATA_200 = loadAndPreprocess(200);
DATA_500 = loadAndPreprocess(500);
DATA_1000 = loadAndPreprocess(1000);
DATA = loadAndPreprocess();

bP = bayesPrediction(DATA[[1]], DATA[[2]])
draw_roc_curve(bP, DATA[[2]][,"party_changed"])

tP = treePrediction(DATA[[1]], DATA[[2]])
draw_roc_curve(tP, DATA[[2]][,"party_changed"])

thP_own_dst = tresholdPrediction(DATA[[1]], DATA[[2]], "own_dst")
draw_roc_curve(thP_own_dst, DATA[[2]][,"party_changed"])

thP_own_dst_norm = tresholdPrediction(DATA[[1]], DATA[[2]], "own_dst_norm")
draw_roc_curve(thP_own_dst_norm, DATA[[2]][,"party_changed"])

thP_own_mode_dst = tresholdPrediction(DATA[[1]], DATA[[2]], "own_mode_dst")
draw_roc_curve(thP_own_mode_dst, DATA[[2]][,"party_changed"])

thP_avg_streak_against_party = tresholdPrediction(DATA[[1]], DATA[[2]], "avg_streak_against_party")
draw_roc_curve(thP_avg_streak_against_party, DATA[[2]][,"party_changed"])

thP_max_streak_against_party = tresholdPrediction(DATA[[1]], DATA[[2]], "max_streak_against_party")
draw_roc_curve(thP_max_streak_against_party, DATA[[2]][,"party_changed"])

##Add another another attributes to +
#decisionTree = ctree(party_changed ~ own_dst + own_mode_dst, data = poslowieMeta)

##example usage to predict whether patry has changed:

##naive bayes


#naive_threshold_result_bin <- as.numeric(naive_threshold_result)
#poslowieMetaBin <- as.numeric(poslowieMeta[,"party_changed"])

#draw_roc_curve(naive_threshold_result_bin, poslowieMetaBin)
