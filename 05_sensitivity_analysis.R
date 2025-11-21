## Sensitivity Analysis 1:
##   Simplified Interaction (IA_status × Any Help-Seeking)
##
## In this analysis, informal and formal help-seeking are merged
## into a single category: "Any help-seeking".
##
## IA_status is reversed so that "Internet addiction" is the
## reference category.
##
## Outcome: mental_health_problems (binary)
## ============================================================

library(lme4)
library(broom.mixed)
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
    IA_status   = Internet.addiction.status,   # reversed IA variable
    HS_status   = Mental.health.help.seeking,
    schoolID    = schoolID
  )

## ------------------------------------------------------------
## 3. Convert categorical variables to factors
## ------------------------------------------------------------

factor_vars <- c(
  "year","gender","site","ethnicity","grade","boarding",
  "chronic_cond","covid","glasses","BMI",
  "IA_status","HS_status","schoolID"
)

data[factor_vars] <- lapply(data[factor_vars], factor)

## ------------------------------------------------------------
## 4. Construct binary help-seeking variable
## ------------------------------------------------------------
## Original HS_status levels:
##   - No help-seeking
##   - Informal help-seeking
##   - Formal help-seeking
##
## New variable:
##   - "No help-seeking"
##   - "Any help-seeking"  (combining informal + formal)

data$HS_any <- ifelse(data$HS_status == "No help-seeking",
                      "No help-seeking",
                      "Any help-seeking")

data$HS_any <- factor(data$HS_any)
data$HS_any <- relevel(data$HS_any, ref = "No help-seeking")

## ------------------------------------------------------------
## 5. Set reference levels for IA_status and covariates
## ------------------------------------------------------------

data$IA_status <- relevel(data$IA_status, ref = "Internet addiction")

data$gender    <- relevel(data$gender, ref = "Boys")
data$site      <- relevel(data$site, ref = "Urban")
data$ethnicity <- relevel(data$ethnicity, ref = "Han")
data$boarding  <- relevel(data$boarding, ref = "No")
data$chronic_cond <- relevel(data$chronic_cond, ref = "0")
data$covid     <- relevel(data$covid, ref = "No")
data$glasses   <- relevel(data$glasses, ref = "No")
data$BMI       <- relevel(data$BMI, ref = "Normal")

## ------------------------------------------------------------
## 6. Fit GLMM with simplified interaction
## ------------------------------------------------------------

model_sensitivity <- glmer(
  mental_health_problems ~ 
    IA_status * HS_any +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

summary(model_sensitivity)

## ------------------------------------------------------------
## 7. Extract OR table
## ------------------------------------------------------------

OR_table <- tidy(model_sensitivity, effects = "fixed") %>%
  mutate(
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

write.csv(OR_table,
          "output/02_sensitivity_anyhelp_OR_table.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## 8. Predicted probabilities for IA_status × HS_any
## ------------------------------------------------------------

emm_sensitivity <- emmeans(model_sensitivity,
                           ~ IA_status * HS_any,
                           type = "response")

plot_df <- as.data.frame(emm_sensitivity)

write.csv(plot_df,
          "output/02_sensitivity_anyhelp_predicted_probabilities.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ============================================================


## Sensitivity Analysis 2:
##   Linear Mixed-Effects Models (LMM)
##   Outcomes:
##     - Depressive symptoms (CESD-20 score)
##     - Anxiety symptoms (GAD-7 score)
##
## Note:
## The analytical structure for GAD-7 is identical to CESD-20.
## Only the dependent variable changes.
## ============================================================

library(lme4)
library(emmeans)
library(broom.mixed)
library(dplyr)

## ------------------------------------------------------------
## 1. Load dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

## ------------------------------------------------------------
## 2. Rename variables (consistent with main analysis)
## ------------------------------------------------------------

data <- data %>%
  rename(
    CESD20      = CESD20_score,
    GAD7        = GAD7_score,
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
    HS_status   = Mental.health.help.seeking,
    schoolID    = schoolID
  )

## ------------------------------------------------------------
## 3. Convert categorical variables to factors
## ------------------------------------------------------------

factor_vars <- c(
  "year","gender","site","ethnicity","grade","boarding",
  "chronic_cond","covid","glasses","BMI",
  "IA_status","HS_status","schoolID"
)

data[factor_vars] <- lapply(data[factor_vars], factor)

## ------------------------------------------------------------
## 4. Set reference levels (must match Table S7)
## ------------------------------------------------------------

## Internet addiction status (two reference codings)
## Model A: Reference = “No Internet use”
data$IA_status_refA <- relevel(data$IA_status, ref = "No Internet use")

## Model B: Reference = “Non-addictive Internet use”
data$IA_status_refB <- relevel(data$IA_status, ref = "Non-addictive Internet use")

## Mental health help-seeking
## Model A: Reference = “No help-seeking”
data$HS_status_refA <- relevel(data$HS_status, ref = "No help-seeking")

## Model B: Reference = “Informal help-seeking”
data$HS_status_refB <- relevel(data$HS_status, ref = "Informal help-seeking")

## Other covariates
data$gender    <- relevel(data$gender, ref = "Boys")
data$site      <- relevel(data$site, ref = "Urban")
data$ethnicity <- relevel(data$ethnicity, ref = "Han")
data$boarding  <- relevel(data$boarding, ref = "No")
data$chronic_cond <- relevel(data$chronic_cond, ref = "0")
data$covid     <- relevel(data$covid, ref = "No")
data$glasses   <- relevel(data$glasses, ref = "No")
data$BMI       <- relevel(data$BMI, ref = "Normal")

## ------------------------------------------------------------
## 5. Fit LMM: CESD20 (continuous)
## ------------------------------------------------------------

model_CESD <- lmer(
  CESD20 ~ IA_status_refA + HS_status_refA +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data
)

summary(model_CESD)

## ------------------------------------------------------------
## 6. Extract coefficients + CI (as Table S7)
## ------------------------------------------------------------

CESD_OR <- tidy(model_CESD) %>%
  mutate(
    CI_low  = estimate - 1.96 * std.error,
    CI_high = estimate + 1.96 * std.error
  )

write.csv(CESD_OR,
          "output/S7_CESD20_main_effects.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## 7. Pairwise comparisons (Netflix-style contrasts)
## ------------------------------------------------------------

emm_addict  <- emmeans(model_CESD, ~ IA_status_refA)
emm_help    <- emmeans(model_CESD, ~ HS_status_refA)

pair_addict <- pairs(emm_addict) %>% summary(infer = TRUE)
pair_help   <- pairs(emm_help) %>% summary(infer = TRUE)

pair_addict <- as.data.frame(pair_addict) %>%
  mutate(variable = "Internet addiction status",
         p_adj_BH = p.adjust(p.value, "BH"))

pair_help <- as.data.frame(pair_help) %>%
  mutate(variable = "Mental health help-seeking",
         p_adj_BH = p.adjust(p.value, "BH"))

pair_results <- bind_rows(pair_addict, pair_help)

write.csv(pair_results,
          "output/S7_CESD20_pairwise.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## END OF SCRIPT
## ============================================================

## Sensitivity Analysis 3:
## Daily Internet Use Duration → Mental Health Problems
##
## GLMM with identical covariates & random-effect structure
## as used in the main analyses.
##
## No pairwise comparisons and no BH correction are applied.
## ============================================================

library(lme4)
library(broom.mixed)
library(dplyr)

## ------------------------------------------------------------
## 1. Load Dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

## ------------------------------------------------------------
## 2. Rename variables to match manuscript
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
    duration    = Daily.internet.use.duration,   # <--- 主要自变量
    schoolID    = schoolID
  )

## ------------------------------------------------------------
## 3. Convert categorical predictors to factors
## ------------------------------------------------------------

factor_vars <- c(
  "year","gender","site","ethnicity","grade","boarding",
  "chronic_cond","covid","glasses","BMI",
  "duration","schoolID"
)
data[factor_vars] <- lapply(data[factor_vars], factor)

## ------------------------------------------------------------
## 4. Set reference levels (consistent with Table S9)
## ------------------------------------------------------------

data$duration <- relevel(data$duration, ref = "No Internet use")
data$gender    <- relevel(data$gender, ref = "Boys")
data$site      <- relevel(data$site, ref = "Urban")
data$ethnicity <- relevel(data$ethnicity, ref = "Han")
data$boarding  <- relevel(data$boarding, ref = "No")
data$chronic_cond <- relevel(data$chronic_cond, ref = "0")
data$covid     <- relevel(data$covid, ref = "No")
data$glasses   <- relevel(data$glasses, ref = "No")
data$BMI       <- relevel(data$BMI, ref = "Normal")

## ------------------------------------------------------------
## 5. Fit GLMM
## ------------------------------------------------------------

model_duration <- glmer(
  mental_health_problems ~
    duration +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

summary(model_duration)

## ------------------------------------------------------------
## 6. Extract Odds Ratios & 95% CI (Table S9)
## ------------------------------------------------------------

duration_OR <- tidy(model_duration, effects = "fixed") %>%
  mutate(
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

write.csv(duration_OR,
          "output/03_OR_duration.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ============================================================

## Sensitivity Analysis 4:
## Joint Categories of Internet Addiction × Help-Seeking
## 9 Exposure Combinations (3 × 3)
## Outcome: mental_health_problems (binary)
## ============================================================

library(lme4)
library(broom.mixed)
library(dplyr)

## ------------------------------------------------------------
## 1. Load Dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

## ------------------------------------------------------------
## 2. Rename variables (consistent with manuscript)
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
    IA_status   = Internet.addiction.status,      # reversed variable
    HS_status   = Mental.health.help.seeking,
    schoolID    = schoolID
  )

## ------------------------------------------------------------
## 3. Convert categorical variables to factors
## ------------------------------------------------------------

factor_vars <- c(
  "year","gender","site","ethnicity","grade","boarding",
  "chronic_cond","covid","glasses","BMI",
  "IA_status","HS_status","schoolID"
)
data[factor_vars] <- lapply(data[factor_vars], factor)

## ------------------------------------------------------------
## 4. Set reference levels
## ------------------------------------------------------------

## Internet addiction reversed → reference = "Internet addiction"
data$IA_status <- relevel(data$IA_status, ref = "Internet addiction")

## Help-seeking reference = "No help-seeking"
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
## 5. Create Joint Exposure Variable (9 categories)
## ------------------------------------------------------------

data$Joint_IA_HS <- interaction(
  data$IA_status,
  data$HS_status,
  sep = "+",
  lex.order = TRUE
)

## Set reference level: "Internet addiction + No help-seeking"
data$Joint_IA_HS <- relevel(
  data$Joint_IA_HS,
  ref = "Internet addiction+No help-seeking"
)

## ------------------------------------------------------------
## 6. Fit GLMM (main model with joint exposure variable)
## ------------------------------------------------------------

model_joint <- glmer(
  mental_health_problems ~ 
    Joint_IA_HS +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data,
  family = binomial(link = "logit"),
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 2e5)
  )
)

summary(model_joint)

## ------------------------------------------------------------
## 7. Extract OR Table (matches Table S10)
## ------------------------------------------------------------

joint_OR <- tidy(model_joint, effects = "fixed") %>%
  mutate(
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

write.csv(joint_OR,
          "output/04_joint_categories_OR_table.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ============================================================
