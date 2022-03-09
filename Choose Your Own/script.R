# Verify if required packages are installed. If they are not, install them
if(!require(foreach)) install.packages("foreach")
if(!require(iterators)) install.packages("iterators")
if(!require(parallel)) install.packages("parallel")
if(!require(doParallel)) install.packages("doParallel")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(data.table)) install.packages("data.table")
if(!require(caret)) install.packages("caret")
if(!require(MASS)) install.packages("MASS")
if(!require(HDclassif)) install.packages("HDclassif")
if(!require(LiblineaR)) install.packages("LiblineaR")
if(!require(e1071)) install.packages("e1071")
if(!require(ranger)) install.packages("ranger")
if(!require(Rborist)) install.packages("Rborist")
if(!require(nnet)) install.packages("nnet")
if(!require(Rcpp)) install.packages("Rcpp")
if(!require(RSNNS)) install.packages("RSNNS")
if(!require(pls)) install.packages("pls")

# Unload the packages that might cover up the namespace used in code
s <- sessionInfo()
if ("pls" %in% names(s$otherPkgs)) detach("package:pls", unload = T)
if ("RSNNS" %in% names(s$otherPkgs)) detach("package:RSNNS", unload = T)
if ("ranger" %in% names(s$otherPkgs)) detach("package:ranger", unload = T)
if ("e1071" %in% names(s$otherPkgs)) detach("package:e1071", unload = T)
if ("LiblineaR" %in% names(s$otherPkgs)) detach("package:LiblineaR", unload = T)
if ("HDclassif" %in% names(s$otherPkgs)) detach("package:HDclassif", unload = T)
rm(s)

# These five packages are required for code to run.
# Other packages will be loaded dynamically as needed by caret.
# This makes sure they're loaded if they were installed above.
library(doParallel)
library(tidyverse)
library(kableExtra)
library(data.table)
library(caret)

# Register parallel backend for faster execution of code on systems with multiple cores
registerDoParallel()

# Function for uniform display of tables
table_style <- function(table) {
  table %>%
    kbl(digits = 2
        , format.args = list(big.mark = ",")
        ) %>%
    kable_styling("striped"
                  , latex_options = c("HOLD_position"
                                      , "striped"
                                      )
                  ) %>%
    row_spec(0
             , bold = T
             )
  }

# Download files from https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
# Create data directory if it doesn't already exist
if (!dir.exists("data/")) {
  dir.create("data")
}

# Download the file with red wines if needed and load it in memory
if (!file.exists("data/red.csv")) {
  dl <- tempfile()
  download.file(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
    , dl
    )
  file.copy(from = dl
            , to = "data/red.csv"
            )
  rm(dl)
}
wine <- fread("data/red.csv")[, type := "red"]

# Download the file with white wines if needed and load it in memory
if (!file.exists("data/white.csv")) {
  dl <- tempfile()
  download.file(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
    , dl
  )
  file.copy(from = dl
            , to = "data/white.csv"
            )
  rm(dl)
}
# White wines are added to the same data.table as the red wines
wine <- rbind(wine
              , fread("data/white.csv")[, type := "white"]
              )

# Make sure column names are correct for use by all algorithms
names(wine) <- make.names(names(wine))

# Create an outcome column with wine quality and type
wine[, quality_type := as.factor(paste(case_when(quality >= 8 ~ "excellent"
                                                 , quality <= 4 ~ "poor"
                                                 , TRUE ~ "normal"
                                                 ), type
                                       , sep = "_"
                                       )
                                 )
     ]

# Set seed to be able to reproduce results
set.seed(2022)

# Create data sets for training and testing
# Select 80% of the data for training and 20% for testing
wine_test_idx <- createDataPartition(wine$quality_type
                                     , 1
                                     , .2
                                     , FALSE
                                     )

# Select the 11 predictors for the training and test sets
wine_train_X <- wine[!wine_test_idx
                     ] [, .SD
                        , .SDcols = !c("quality"
                                       , "type"
                                       , "quality_type"
                                       )
                        ]

wine_test_X <- wine[wine_test_idx
                    ] [, .SD
                       , .SDcols = !c("quality"
                                      , "type"
                                      , "quality_type"
                                      )
                       ]

# Select the response variable for the training and test sets
wine_train_y <- wine[!wine_test_idx
                     , quality_type
                     ]

wine_test_y <- wine[wine_test_idx
                    , quality_type
                    ]

# Pre-processing settings for caret
pre_process <- c("center"
                 , "scale"
                 )

# Train and evaluate a KNN model
# expand.grid creates a data.frame of all combinations of the options given
knn_tuning <- expand.grid(k = 30)
# Train the model using caret's train method
wine_mod_knn <- train(x = wine_train_X
                      , y = wine_train_y
                      , method = "knn"
                      , tuneGrid = knn_tuning
                      , preProcess = pre_process
                      )
# Predict outcome on the test set
pr_knn <- predict(wine_mod_knn, wine_test_X)
# Evaluate accuracy on the test set
acc_knn <- mean(pr_knn == wine_test_y)
# Create confusion matrix to evaluate the model
cf_knn <- confusionMatrix(pr_knn, wine_test_y)

# Train and evaluate a Partial Least Squares model
pls_tuning <- expand.grid(ncomp = 8)
wine_mod_pls <- train(x = wine_train_X
                      , y = wine_train_y
                      , method = "kernelpls"
                      , tuneGrid = pls_tuning
                      , preProcess = pre_process
                      )
pr_pls <- predict(wine_mod_pls, wine_test_X)
acc_pls <- mean(pr_pls == wine_test_y)
cf_pls <- confusionMatrix(pr_pls, wine_test_y)

# Train and evaluate a Discriminant Analysis model
da_tuning <- expand.grid(threshold = c(.999)
                          , model = c(13)
                          )
wine_mod_da <- train(x = wine_train_X
                      , y = wine_train_y
                      , method = "hdda"
                      , tuneGrid = da_tuning
                      , preProcess = pre_process
                      )
pr_da <- predict(wine_mod_da, wine_test_X) 
acc_da <- mean(pr_da == wine_test_y)
cf_da <- confusionMatrix(pr_da, wine_test_y)

# Train and evaluate an SVM model
svm_tuning <- expand.grid(cost = c(1)
                          , Loss = 0
                          )
wine_mod_svm <- train(x = wine_train_X
                      , y = wine_train_y
                      , method = "svmLinear3"
                      , tuneGrid = svm_tuning
                      , preProcess = pre_process
                      )
pr_svm <- predict(wine_mod_svm, wine_test_X)
acc_svm <- mean(pr_svm == wine_test_y)
cf_svm <- confusionMatrix(pr_svm, wine_test_y)

# train and evaluate a random forest model
rf_tuning <- expand.grid(mtry = 2
                         , splitrule = c("extratrees")
                         , min.node.size = c(1)
                         )
wine_mod_rf <- train(x = wine_train_X
                     , y = wine_train_y
                     , method = "ranger"
                     , tuneGrid = rf_tuning
                     , preProcess = pre_process
                     )
pr_rf <- predict(wine_mod_rf, wine_test_X)
acc_rf <- mean(pr_rf == wine_test_y)
cf_rf <- confusionMatrix(pr_rf, wine_test_y)

# Train and evaluate a 2nd random forest model
rf_2_tuning <- expand.grid(predFixed = 2
                           , minNode = 1
                           )
wine_mod_rf_2 <- train(x = wine_train_X
                       , y = wine_train_y
                       , method = "Rborist"
                       , tuneGrid = rf_2_tuning
                       , preProcess = pre_process
                       )
pr_rf_2 <- predict(wine_mod_rf_2, wine_test_X)
acc_rf_2 <- mean(pr_rf_2 == wine_test_y)
cf_rf_2 <- confusionMatrix(pr_rf_2, wine_test_y)

# Train and evaluate a single layer neural net model
nn_tuning <- expand.grid(size = c(24)
                         , decay = c(0.01)
                         , bag = c(F)
                         )
wine_mod_nn <- train(x = wine_train_X
                     , y = wine_train_y
                     , method = "avNNet"
                     , tuneGrid = nn_tuning
                     , preProcess = pre_process
                     )
pr_nn <- predict(wine_mod_nn, wine_test_X)
acc_nn <- mean(pr_nn == wine_test_y)
cf_nn <- confusionMatrix(pr_nn, wine_test_y)

# Train and evaluate a multi layer nn model
ml_nn_tuning <- expand.grid(layer1 = c(14)
                            , layer2 = c(9)
                            , layer3 = c(6)
                            )
wine_mod_ml_nn <- train(x = wine_train_X
                        , y = wine_train_y
                        , method = "mlpML"
                        , tuneGrid = ml_nn_tuning
                        , preProcess = pre_process
                        )
pr_ml_nn <- predict(wine_mod_ml_nn, wine_test_X)
acc_ml_nn <- mean(pr_ml_nn == wine_test_y)
cf_ml_nn <- confusionMatrix(pr_ml_nn, wine_test_y)