## NOTE:
## This script analyses the primary binary outcome: mental_health_problems.
## The same model structure was applied to two additional binary outcomes:
##   - depression
##   - anxiety
## Only the dependent variable changes; all covariates and model settings remain identical.
## ============================================================
## 01_main_effect_GLMM.R
## Generalized Linear Mixed-Effects Models (GLMM)
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
## 2. Rename variables
## ------------------------------------------------------------

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
## 3b. Set reference levels (must match original analysis)
## ------------------------------------------------------------

data$IA_status <- relevel(data$IA_status, ref = "No Internet use")
data$HS_status <- relevel(data$HS_status, ref = "No help-seeking")
data$gender    <- relevel(data$gender, ref = "Boys")
data$site      <- relevel(data$site, ref = "Urban")
data$ethnicity <- relevel(data$ethnicity, ref = "Han")
data$boarding  <- relevel(data$boarding, ref = "No")
data$chronic_cond <- relevel(data$chronic_cond, ref = "0")
data$covid     <- relevel(data$covid, ref = "No")
data$glasses   <- relevel(data$glasses, ref = "No")
data$BMI       <- relevel(data$BMI, ref = "Normal")

## ------------------------------------------------------------
## 4. Fit GLMM (main effects model)
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
## 6. Marginal predicted probabilities
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
## pairwise_GLMM.R
## Pairwise comparisons for IA_status and HS_status
## ============================================================

library(lme4)
library(emmeans)
library(dplyr)

## ------------------------------------------------------------
## 1. Load Dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

## ------------------------------------------------------------
## 2. Rename variables
## ------------------------------------------------------------

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
## 3. Set reference levels (same as main model)
## ------------------------------------------------------------

data$IA_status <- relevel(data$IA_status, ref = "No Internet use")
data$HS_status <- relevel(data$HS_status, ref = "No help-seeking")
data$gender    <- relevel(data$gender, ref = "Boys")
data$site      <- relevel(data$site, ref = "Urban")
data$ethnicity <- relevel(data$ethnicity, ref = "Han")
data$boarding  <- relevel(data$boarding, ref = "No")
data$chronic_cond <- relevel(data$chronic_cond, ref = "0")
data$covid     <- relevel(data$covid, ref = "No")
data$glasses   <- relevel(data$glasses, ref = "No")
data$BMI       <- relevel(data$BMI, ref = "Normal")

## ------------------------------------------------------------
## 4. Fit GLMM (same structure as main model)
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
## 5. Pairwise comparisons
## ------------------------------------------------------------

run_pairwise <- function(model, var, adjust_method = "BH") {
  emm <- emmeans(model, as.formula(paste("~", var)), type = "link")
  contr <- contrast(emm, "pairwise", adjust = adjust_method)
  df <- as.data.frame(summary(contr, infer = TRUE)) %>%
    mutate(
      OR = exp(estimate),
      CI_low  = exp(estimate - 1.96 * SE),
      CI_high = exp(estimate + 1.96 * SE),
      variable = var
    )
  return(df)
}

vars_to_compare <- c("IA_status", "HS_status")

pairwise_results <- bind_rows(
  lapply(vars_to_compare, function(v) run_pairwise(model, v))
)

write.csv(pairwise_results, "output/pairwise_comparisons.csv", row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ------------------------------------------------------------
