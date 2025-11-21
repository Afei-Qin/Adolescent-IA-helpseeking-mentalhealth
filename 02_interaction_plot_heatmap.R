## ============================================================
## 02_interaction_GLMM.R
## GLMM with Interaction: IA_status × HS_status
## (Internet addiction variable reversed so that
##   "Internet addiction" is the reference category)
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
    IA_status   = Internet.addiction.status,  # reversed variable
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
## IMPORTANT:
## IA_status has been reversed so that the reference = "Internet addiction"
## HS_status unchanged (reference = "No help-seeking")

data$IA_status <- relevel(data$IA_status, ref = "Internet addiction")
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
## 5. Fit GLMM with interaction IA_status × HS_status
## ------------------------------------------------------------

model_interaction <- glmer(
  mental_health_problems ~ 
    IA_status * HS_status +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI +
    (1 | schoolID),
  data = data,
  family = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5))
)

summary(model_interaction)

## ------------------------------------------------------------
## 6. Extract OR table (matches Table 3 format)
## ------------------------------------------------------------

interaction_OR <- tidy(model_interaction, effects = "fixed") %>%
  mutate(
    OR      = exp(estimate),
    CI_low  = exp(estimate - 1.96 * std.error),
    CI_high = exp(estimate + 1.96 * std.error)
  )

write.csv(interaction_OR,
          "output/02_interaction_OR_table.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## 7. Predicted probabilities for line plot & heatmap
## ------------------------------------------------------------

emm_interaction <- emmeans(model_interaction,
                           ~ IA_status * HS_status,
                           type = "response")

plot_df <- as.data.frame(emm_interaction)

write.csv(plot_df,
          "output/02_predicted_probabilities_interaction.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ============================================================

