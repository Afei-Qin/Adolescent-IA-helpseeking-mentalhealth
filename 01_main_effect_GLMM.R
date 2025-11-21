## NOTE:
## This script analyses the primary binary outcome: mental_health_problems.
## The same model structure was applied to two additional binary outcomes:
##   - depression
##   - anxiety
## Only the dependent variable changes; all covariates and model settings remain identical.
## ============================================================
## 01_main_effect_GLMM.R
## Generalized Linear Mixed-Effects Models (GLMM)
## Outcome: Mental Health Problems (binary)
## ============================================================

library(lme4)
library(emmeans)
library(broom.mixed)
library(dplyr)

## ------------------------------------------------------------
## 1. Load Dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

## ------------------------------------------------------------
## 2. Rename variables (mapping your original dataset)
## ------------------------------------------------------------

data <- data %>%
  rename(
    mental_health_problems = mental_health_problems,   # binary outcome (0/1)
    year        = Year,
    gender      = Gender,
    age         = Age,
    site        = Monitoring.site,
    ethnicity   = Ethnic,
    grade       = Grade,
    boarding    = Board.at.school,
    chronic_cond = Number.of.chronic.disease.conditions,
    covid       = COVID.19.infection,
    glasses     = To.wear.glasses,
    BMI         = BMI,
    IA_status   = Internet.addiction.status,
    HS_status   = Mental.health.help.seeking
  )

## ------------------------------------------------------------
## 3. Convert categorical variables to factors
## ------------------------------------------------------------

factor_vars <- c(
  "year", "gender", "site", "ethnicity", "grade", "boarding",
  "chronic_cond", "covid", "glasses", "BMI",
  "IA_status", "HS_status", "schoolID"
)

data[factor_vars] <- lapply(data[factor_vars], factor)

## ------------------------------------------------------------
## 4. Fit GLMM (main effects + interaction)
## ------------------------------------------------------------

model_glmm <- glmer(
  mental_health_problems ~ 
    IA_status + HS_status +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

summary(model_glmm)

## ------------------------------------------------------------
## 5. Extract OR Table
## ------------------------------------------------------------

OR_table <- tidy(model_glmm, effects = "fixed") %>%
  mutate(
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

write.csv(OR_table, "output/01_OR_main_effects.csv", row.names = FALSE)

## ------------------------------------------------------------
## 6. Marginal predicted probabilities (for plots)
## ------------------------------------------------------------

emm <- emmeans(model_glmm,
               ~ IA_status * HS_status,
               type = "response")

plot_df <- as.data.frame(emm)
write.csv(plot_df, "output/01_predicted_probabilities.csv", row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ------------------------------------------------------------

## Multiple comparisons for IA_status and HS_status
## ============================================================
## 04_pairwise_GLMM.R
## Pairwise comparisons for multi-level categorical predictors
## ============================================================

library(lme4)
library(emmeans)
library(dplyr)

## ------------------------------------------------------------
## 1. Load model or dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

# Ensure same variable names as 01_main_effect_GLMM.R
data <- data %>%
  rename(
    mental_health_problems = mental_health_problems,
    year        = Year,
    gender      = Gender,
    age         = Age,
    site        = Monitoring.site,
    ethnicity   = Ethnic,
    grade       = Grade,
    boarding    = Board.at.school,
    chronic_cond = Number.of.chronic.disease.conditions,
    covid       = COVID.19.infection,
    glasses     = To.wear.glasses,
    BMI         = BMI,
    IA_status   = Internet.addiction.status,
    HS_status   = Mental.health.help.seeking
  )

factor_vars <- c(
  "year", "gender", "site", "ethnicity", "grade", "boarding",
  "chronic_cond", "covid", "glasses", "BMI",
  "IA_status", "HS_status", "schoolID"
)
data[factor_vars] <- lapply(data[factor_vars], factor)

## ------------------------------------------------------------
## 2. Fit model (same structure as main model)
## ------------------------------------------------------------

model <- glmer(
  mental_health_problems ~ 
    IA_status + HS_status +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

## ------------------------------------------------------------
## 3. Pairwise contrast function
## ------------------------------------------------------------

run_pairwise <- function(model, var, adjust_method = "BH") {
  
  emm <- emmeans(model, as.formula(paste("~", var)), type = "link")
  
  contr <- contrast(emm, "pairwise", adjust = adjust_method)
  
  df <- as.data.frame(summary(contr, infer = TRUE)) %>%
    mutate(
      OR       = exp(estimate),
      CI_low   = exp(estimate - 1.96 * SE),
      CI_high  = exp(estimate + 1.96 * SE),
      variable = var
    )
  
  return(df)
}

## ------------------------------------------------------------
## 4. Run pairwise comparisons
## ------------------------------------------------------------

vars_to_compare <- c("IA_status", "HS_status")

pairwise_results <- lapply(vars_to_compare, function(v) run_pairwise(model, v))
pairwise_results <- bind_rows(pairwise_results)

write.csv(pairwise_results, "output/04_pairwise_comparisons.csv", row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ------------------------------------------------------------

