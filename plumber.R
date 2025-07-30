library(plumber)
library(readr)
library(dplyr)
library(caret)

data <- readRDS("clean_data.rds")

set.seed(123)
train_index <- createDataPartition(data$diabetes_binary, p = 0.7, list = FALSE)
train <- data[train_index, ]
test <- data[-train_index, ]

# best model
best_model <- train(
  diabetes_binary ~ bmi + high_bp + gen_hlth,
  data = train,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = mnLogLoss),
  metric = "logLoss"
)

#* @apiTitle Diabetes Prediction API

#* return info
#* @get /info
function() {
  list(
    name = "Julia Fish",
    github_pages_url = "https://juliafish828.github.io/Project-3"
  )
}

#* predict
#* @param bmi:double
#* @param high_bp:string
#* @param gen_hlth:int
#* @get /pred
function(bmi = mean(train$bmi, na.rm = TRUE),
         high_bp = names(sort(table(train$high_bp), decreasing = TRUE))[1],
         gen_hlth = as.integer(names(sort(table(train$gen_hlth), decreasing = TRUE))[1])) {
  
  new_data <- data.frame(
    bmi = as.numeric(bmi),
    high_bp = as.integer(high_bp),
    gen_hlth = as.integer(gen_hlth)
  )
  
  probs <- predict(best_model, new_data, type = "prob")
  return(probs)
}

