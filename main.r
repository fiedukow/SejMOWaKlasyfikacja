library("party") ##library with decision tree
library("e1071") ##library with naive bayes
library("ROCR") ##library with roc curve

source("statistics_generators.r")
source("treshold_prediction.r")
source("roc_drawing.r")
source('load_and_preprocess.r')

#### BUILDING THE MODEL
DATA_50 = loadAndPreprocess(50);
DATA_100 = loadAndPreprocess(100);
DATA_200 = loadAndPreprocess(200);
DATA_500 = loadAndPreprocess(500);
DATA_1000 = loadAndPreprocess(1000);
DATA = loadAndPreprocess();

##Add another another attributes to +
#decisionTree = ctree(party_changed ~ own_dst + own_mode_dst, data = poslowieMeta)

##example usage to predict whether patry has changed:
#predict(decisionTree, newdata = half_poslowieMeta) # newdata should be data, which we want to predict party_changed (also it should have computed attributes)

##naive bayes
#bayes = naiveBayes(as.factor(party_changed) ~ own_dst + own_mode_dst, data=poslowieMeta) ##predict works same as for decistion tree

#naive_threshold_result_bin <- as.numeric(naive_threshold_result)
#poslowieMetaBin <- as.numeric(poslowieMeta[,"party_changed"])

#draw_roc_curve(naive_threshold_result_bin, poslowieMetaBin)
