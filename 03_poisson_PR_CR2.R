## ============================================================
## poisson_PR_CR2.R
## Robust Poisson Regression with Cluster-Robust (CR2) SEs
##
## NOTE:
## This script fits Poisson models with log link to estimate 
## prevalence ratios (PRs) for the main outcome:
##   - mental_health_problems
## The same model structure was also applied to:
##   - depression
##   - anxiety
## Only the dependent variable changes; all covariates and
## model settings remain identical.
## ============================================================

library(dplyr)
library(lme4)
library(clubSandwich)

## ------------------------------------------------------------
## 1. Load dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

## ------------------------------------------------------------
## 2. Rename variables (consistent with main analysis)
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
    IA_status   = Internet.addiction.status,   # reversed later
    HS_status   = Mental.health.help.seeking,
    schoolID    = schoolID,
    depression  = Depression,
    anxiety     = Anxiety
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
## 5. Fit robust Poisson model (main outcome)
## ------------------------------------------------------------

model_pois_mhp <- glm(
  mental_health_problems ~ 
    IA_status * HS_status +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI,
  family = poisson(link = "log"),
  data = data
)

## Cluster-robust (CR2) SEs by school
robust_mhp <- coef_test(model_pois_mhp,
                        vcov = "CR2",
                        cluster = data$schoolID)

write.csv(robust_mhp,
          "output/PR_CR2_mental_health_problems.csv",
          row.names = FALSE)


## ------------------------------------------------------------
## 6. Additional outcomes: depression & anxiety
## ------------------------------------------------------------
## Same model, only change the dependent variable

### Depression
model_pois_dep <- glm(
  depression ~ 
    IA_status * HS_status +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI,
  family = poisson(link = "log"),
  data = data
)

robust_dep <- coef_test(model_pois_dep,
                        vcov = "CR2",
                        cluster = data$schoolID)

write.csv(robust_dep,
          "output/PR_CR2_depression.csv",
          row.names = FALSE)


### Anxiety
model_pois_anx <- glm(
  anxiety ~ 
    IA_status * HS_status +
    year + gender + age + site + ethnicity + grade + boarding +
    chronic_cond + covid + glasses + BMI,
  family = poisson(link = "log"),
  data = data
)

robust_anx <- coef_test(model_pois_anx,
                        vcov = "CR2",
                        cluster = data$schoolID)

write.csv(robust_anx,
          "output/PR_CR2_anxiety.csv",
          row.names = FALSE)

## ------------------------------------------------------------
## End of Script
## ============================================================

