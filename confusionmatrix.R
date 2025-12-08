create_cm <- function(pred_df, t_pre, t_diab, test) {
  
  predicted_three <- with(pred_df, 
                          ifelse(PreDiabetes + Diabetes < t_pre, "NoDiabetes",
                                 ifelse(Diabetes >= t_diab, "Diabetes", "PreDiabetes"))
  )
  
  predicted_three <- factor(predicted_three, 
                            levels = c("NoDiabetes", "PreDiabetes", "Diabetes"))
  
  cm <- confusionMatrix(predicted_three, test$Diabetes_012)
  
  return(cm)
}

plot_cm <- function(cm) {
  cm_df <- as.data.frame(cm$table)
  names(cm_df) <- c("Predicted", "Actual", "Freq")
  
  ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      title = "Confusion Matrix (Shaded by Frequency)",
      x = "Actual Class",
      y = "Predicted Class",
      fill = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}