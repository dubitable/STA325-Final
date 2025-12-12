library(dplyr)

cleandata = function(data) {
  num_cols <- c("BMI", "MentHlth", "PhysHlth")
  for (col in names(data)) {
    if (!(col %in% num_cols)) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  data <- data |>
    mutate(
      Diabetes_012 = factor(Diabetes_012,
                            levels = c(0, 1, 2),
                            labels = c("NoDiabetes", "PreDiabetes", "Diabetes"),
                            ordered = TRUE
      )
    )
  data$Age3 <- cut(as.numeric(as.character(data$Age)),
                 breaks = c(0, 4, 9, 13),
                 labels = c("Young", "Middle", "Old"),
                 ordered_result = FALSE)
  
  data$logBMI <- log(data$BMI)
  data$logBMI_c <- (data$logBMI - mean(data$logBMI)) / sd(data$logBMI)
  return(data)
}