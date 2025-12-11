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
  return(data)
}