library(gridExtra)
library(MASS)
library(brant)

source("./cleandata.R")
source("./partition.R")

diabetes_raw <- read.csv("data/diabetes_012_health_indicators_BRFSS2015.csv")
diabetes <- cleandata(diabetes_raw)

split <- partition(diabetes, 0.8)
train <- split$train
test <- split$test

model_polr <- polr(
  Diabetes_012 ~ 
    logBMI_c + Age3 + HighBP + HighChol + 
    Smoker + Stroke + HeartDiseaseorAttack + 
    HvyAlcoholConsump + AnyHealthcare + NoDocbcCost + GenHlth + DiffWalk + Sex + Income  +
    HighBP:HighChol,
  data = train,
  method = "logistic",
  Hess = TRUE
)

distribution_plot <- function() {
  cat_vars <- c("Age", "HighBP", "HighChol", "Smoker", 
                "HeartDiseaseorAttack", "HvyAlcoholConsump", 
                "GenHlth", "Income", "Sex")
  
  diabetes |>
    dplyr::select(all_of(cat_vars), Diabetes_012) |>
    pivot_longer(cols = all_of(cat_vars), names_to = "variable", values_to = "value") |>
    ggplot(aes(x = value, fill = Diabetes_012)) +
    geom_bar(position = "fill") +                  # stack normalized to proportions
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~variable, scales = "free") +
    labs(
      y = "Proportion of Diabetes Categories",
      x = NULL,
      fill = "Diabetes Risk",
      title = ""
    ) +
    theme_minimal()
}

results_plot <- function() {
  pred <- predict(model_polr, newdata = test, type = "probs")
  predclass <- predict(model_polr, newdata = test, type = "class")
  pred_df <- as.data.frame(pred)
  
  t_pre <- 0.5
  t_diab <- 0.3
  
  cm <- create_cm(pred_df, t_pre, t_diab, test)
  plot_cm(cm, "")
}


healthy_template <- function() {
  template <- model_polr$model[1, , drop = FALSE]
  
  template$Age3 <- factor("Young", levels = levels(model_polr$model$Age3)) 
  template$HighBP <- factor(0, levels = levels(model_polr$model$HighBP)) # 0 no high BP
  template$HighChol <- factor(0, levels = levels(model_polr$model$HighChol)) # 0 no high Chol
  template$PhysActivity <- factor(0, levels = levels(model_polr$model$PhysActivity)) # 0 no phys ac
  template$Smoker <- factor(0, levels = levels(model_polr$model$Smoker)) # 0 no smoker
  template$Stroke <- factor(0, levels = levels(model_polr$model$Stroke)) # 0 no stroke
  template$HeartDiseaseorAttack <- factor(1, levels = levels(model_polr$model$HeartDiseaseorAttack))#  0 no disease
  template$HvyAlcoholConsump <- factor(0, levels = levels(model_polr$model$HvyAlcoholConsump)) # 0 no alcohol
  template$AnyHealthcare <- factor(0, levels = levels(model_polr$model$AnyHealthcare)) # 0 no healthcare
  template$NoDocbcCost <- factor(0, levels = levels(model_polr$model$NoDocbcCost)) # 0 no doctor cost
  template$GenHlth <- factor(1, levels = levels(model_polr$model$GenHlth)) # 1-5 1 excellent
  template$DiffWalk <- factor(1, levels = levels(model_polr$model$DiffWalk)) # 0 no hard time climbing
  template$Sex <- factor(0, levels = levels(model_polr$model$Sex)) # 0 female
  template$Income <- factor(1, levels = levels(model_polr$model$Income)) # 1-8, 1 poor
  
  return(template)
}

unhealthy_template <- function() {
  template <- model_polr$model[1, , drop = FALSE]
  
  template$Age3 <- factor("Young", levels = levels(model_polr$model$Age3)) 
  template$HighBP <- factor(1, levels = levels(model_polr$model$HighBP)) # 0 no high BP
  template$HighChol <- factor(1, levels = levels(model_polr$model$HighChol)) # 0 no high Chol
  template$PhysActivity <- factor(1, levels = levels(model_polr$model$PhysActivity)) # 0 no phys ac
  template$Smoker <- factor(1, levels = levels(model_polr$model$Smoker)) # 0 no smoker
  template$Stroke <- factor(1, levels = levels(model_polr$model$Stroke)) # 0 no stroke
  template$HeartDiseaseorAttack <- factor(1, levels = levels(model_polr$model$HeartDiseaseorAttack))#  0 no disease
  template$HvyAlcoholConsump <- factor(1, levels = levels(model_polr$model$HvyAlcoholConsump)) # 0 no alcohol
  template$AnyHealthcare <- factor(0, levels = levels(model_polr$model$AnyHealthcare)) # 0 no healthcare
  template$NoDocbcCost <- factor(1, levels = levels(model_polr$model$NoDocbcCost)) # 0 no doctor cost
  template$GenHlth <- factor(3, levels = levels(model_polr$model$GenHlth)) # 1-5 1 excellent
  template$DiffWalk <- factor(1, levels = levels(model_polr$model$DiffWalk)) # 0 no hard time climbing
  template$Sex <- factor(0, levels = levels(model_polr$model$Sex)) # 0 female
  template$Income <- factor(1, levels = levels(model_polr$model$Income)) # 1-8, 1 poor
  
  return(template)
}

get_probsBMI <- function(model, df_new) {
  vary_seq = seq(min(diabetes$BMI), max(diabetes$BMI), by = 0.5)
  df_new <- df_new[rep(1, length(vary_seq)), , drop = FALSE]
  df_new[["BMI"]] <- vary_seq
  
  df_new[["logBMI_c"]] <- (log(df_new[["BMI"]]) - mean(log(diabetes$BMI))) / sd(log(diabetes$BMI))
  
  probs <- cbind(df_new, predict(model, newdata = df_new, type = "probs"))
  
  probs_long <- probs |>
    pivot_longer(cols = c(NoDiabetes, PreDiabetes, Diabetes),
                 names_to = "Outcome",
                 values_to = "Probability")
  
  probs_long$Outcome <- factor(probs_long$Outcome, 
                               levels = c("NoDiabetes",
                                          "PreDiabetes",
                                          "Diabetes"))
  
  return(probs_long)
}

age_plot <- function() {
  template <- unhealthy_template();
  template$Age3 <- factor("Young", levels = levels(model_polr$model$Age3)) 
  probs1 <- get_probsBMI(model_polr, template)
  probs1$AgeGroup = rep("Age 18-34", nrow(probs1))
  
  template$Age3 <- factor("Middle", levels = levels(model_polr$model$Age3)) 
  probs2 <- get_probsBMI(model_polr, template)
  probs2$AgeGroup = rep("Age 35-64", nrow(probs2))
  
  template$Age3 <- factor("Old", levels = levels(model_polr$model$Age3)) 
  probs3 <- get_probsBMI(model_polr, template)
  probs3$AgeGroup = rep("Age 65+", nrow(probs3))
  
  probs <- rbind(probs1, probs2, probs3)
  
  ggplot(probs, aes(x = BMI, y = Probability, color = Outcome)) +
    geom_line() +
    labs(title = "", color = "") +
    theme(legend.position = "right") + 
    facet_wrap(~AgeGroup)
}

sex_plot <- function() {
  template <- unhealthy_template();
  
  template$Sex <- factor(0, levels = levels(model_polr$model$Sex)) 
  probs1 <- get_probsBMI(model_polr, template)
  probs1$Sex = rep("Female", nrow(probs1))
  probs1$Baseline = rep("Unhealthy", nrow(probs1))
  
  template$Sex <- factor(1, levels = levels(model_polr$model$Sex)) 
  probs2 <- get_probsBMI(model_polr, template)
  probs2$Sex = rep("Male", nrow(probs2))
  probs2$Baseline = rep("Unhealthy", nrow(probs2))
  
  template <- healthy_template();
  
  template$Sex <- factor(0, levels = levels(model_polr$model$Sex)) 
  probs3 <- get_probsBMI(model_polr, template)
  probs3$Sex = rep("Female", nrow(probs3))
  probs3$Baseline = rep("Healthy", nrow(probs3))
  
  template$Sex <- factor(1, levels = levels(model_polr$model$Sex)) 
  probs4 <- get_probsBMI(model_polr, template)
  probs4$Sex = rep("Male", nrow(probs4))
  probs4$Baseline = rep("Healthy", nrow(probs4))

  probs <- rbind(probs1, probs2, probs3, probs4)
  
  ggplot(probs, aes(x = BMI, y = Probability, color = Outcome)) +
    geom_line() +
    labs(title = "", color = "") +
    theme(legend.position = "right") + 
    facet_grid(rows = vars(Baseline), cols = vars(Sex))
}

get_probsbpchol <- function(model, df_new) {
  df_new$logBMI_c <- (log(40) - mean(log(diabetes$BMI))) / sd(log(diabetes$BMI))
  df_new <- df_new[rep(1, 2), , drop = FALSE]
  
  pred <- predict(model, newdata = df_new, type = "probs")

  
  probs <- cbind(df_new, pred)
  
  
  probs_long <- probs |>
    pivot_longer(cols = c(NoDiabetes, PreDiabetes, Diabetes),
                 names_to = "Outcome",
                 values_to = "Probability")
  
  probs_long$Outcome <- factor(probs_long$Outcome, 
                               levels = c("NoDiabetes",
                                          "PreDiabetes",
                                          "Diabetes"))
  
  return(probs_long)
}

brant_plot <- function() {
  tmp <- capture.output(
    brant.result <- as.data.frame(brant(model_polr))
  )
  
  brant_plotdf <- brant.result %>%
    mutate(variable = rownames(.)) %>%
    filter(variable != "Omnibus")
  
  ggplot(brant_plotdf, aes(x = reorder(variable, probability),
                           y = probability)) +
    geom_point(aes(color = probability < 0.05), size = 3) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    coord_flip() +
    scale_y_continuous(sec.axis = dup_axis(name = NULL),
                       limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "",
         x = NULL, y = "p-value", color = "p < 0.05") +
    theme_minimal()
}

bpchol_plot <- function() {
  template <- unhealthy_template();
  
  template$HighChol <- factor(0, levels = levels(model_polr$model$HighChol))
  template$HighBP <- factor(0, levels = levels(model_polr$model$HighBP)) 
  probs1 <- get_probsbpchol(model_polr, template)
  
  template$HighChol <- factor(1, levels = levels(model_polr$model$HighChol))
  template$HighBP <- factor(0, levels = levels(model_polr$model$HighBP)) 
  probs2 <- get_probsbpchol(model_polr, template)
  
  template$HighChol <- factor(0, levels = levels(model_polr$model$HighChol))
  template$HighBP <- factor(1, levels = levels(model_polr$model$HighBP)) 
  probs3 <- get_probsbpchol(model_polr, template)
  
  template$HighChol <- factor(1, levels = levels(model_polr$model$HighChol))
  template$HighBP <- factor(1, levels = levels(model_polr$model$HighBP)) 
  probs4 <- get_probsbpchol(model_polr, template)
  
  probs <- rbind(probs1, probs2, probs3, probs4)
  
  ggplot(probs, aes(x = 0, y = Probability, fill = Outcome)) +
    geom_bar(stat = "identity") +
    labs(title = "", color = "") +
    theme(legend.position = "top") + 
    facet_grid(rows = vars(HighChol), cols = vars(HighBP))
}
