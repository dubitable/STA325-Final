library(tidymodels)
library(themis)

partition <- function(data, p) {
  trainidx <- createDataPartition(data$Diabetes_012, p=p, list=FALSE)
  
  rec <- recipe(Diabetes_012 ~ ., data = data[trainidx, ]) |>
    step_downsample(Diabetes_012)
  
  rec_prep <- prep(rec)
  
  train <- bake(rec_prep, new_data = NULL)
  test <- data[-trainidx,]
  
  return(list(train=train, test=test))
}