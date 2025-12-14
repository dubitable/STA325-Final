get_preds <- function(pred_df, t_pre, t_diab, test) {
  predicted_three <- with(pred_df, 
                          ifelse(PreDiabetes + Diabetes < t_pre, "NoDiabetes",
                                 ifelse(Diabetes >= t_diab, "Diabetes", "PreDiabetes"))
  )
  
  predicted_three <- factor(predicted_three, 
                            levels = c("NoDiabetes", "PreDiabetes", "Diabetes"))
  return(predicted_three)
}

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

plot_cm <- function(cm, title = "Confusion Matrix (Shaded by % of Actual Class)") {
  cm_df <- as.data.frame(cm$table)
  names(cm_df) <- c("Predicted", "Actual", "Freq")
  
  cm_df <- cm_df |>
    group_by(Actual) |>
    mutate(Percent = Freq / sum(Freq))
  
  ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Percent)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%d\n(%.1f%%)", Freq, Percent * 100)),
              color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(
      title = title,
      x = "Actual Class",
      y = "Predicted Class",
      fill = "% of Actual"
    ) + 
    theme_minimal() +
    theme(plot.margin = margin(0, 0, 0, 0))
}