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

PredictAndDrawAll(DATA_50, "roc_n_50", main="ROC Curves, N=50")
PredictAndDrawAll(DATA_100, "roc_n_100", main="ROC Curves, N=100")
PredictAndDrawAll(DATA_200, "roc_n_200", main="ROC Curves, N=200")
PredictAndDrawAll(DATA_500, "roc_n_500", main="ROC Curves, N=500")
PredictAndDrawAll(DATA_1000, "roc_n_1000", main="ROC Curves, N=1000")
PredictAndDrawAll(DATA, "roc_n_inf", main="ROC Curves, N=Inf")
