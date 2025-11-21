## ============================================================
## 04_pairwise_mixed_logit.R
## Pairwise GLMM comparisons for unordered 3-level outcome:
##     help-seeking (No help / Informal / Formal)
##
## This script:
##   1. Fits three pairwise mixed-effects logistic models
##   2. Uses emmeans to estimate marginal means
##   3. Computes pairwise ORs (BH-adjusted)
##   4. Outputs model-level and pairwise-level results
## ============================================================

library(lme4)
library(emmeans)
library(broom.mixed)
library(dplyr)

## ------------------------------------------------------------
## 1. Load and prepare dataset
## ------------------------------------------------------------

data <- read.csv("data/minimal_dataset.csv")

data <- data %>%
  rename(
    help_status = Mental.health.help.seeking,
    IA_status   = Internet.addiction.status,
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
    BMI         = BMI
  )

## Convert to factors
factor_vars <- c("help_status","IA_status","year","gender","site","ethnicity",
                 "grade","boarding","chronic_cond","covid","glasses","BMI","schoolID")
data[factor_vars] <- lapply(data[factor_vars], factor)

## Set consistent reference levels
data$help_status <- relevel(data$help_status, ref = "No help-seeking")
data$IA_status   <- relevel(data$IA_status, ref   = "No Internet use")
data$gender      <- relevel(data$gender,    ref   = "Boys")
data$site        <- relevel(data$site,      ref   = "Urban")
data$ethnicity   <- relevel(data$ethnicity, ref   = "Han")
data$boarding    <- relevel(data$boarding,  ref   = "No")
data$chronic_cond <- relevel(data$chronic_cond, ref = "0")
data$covid       <- relevel(data$covid,     ref   = "No")
data$glasses     <- relevel(data$glasses,   ref   = "No")
data$BMI         <- relevel(data$BMI,       ref   = "Normal")

## Covariates
covariates <- c("IA_status","year","gender","age","site","ethnicity",
                "grade","boarding","chronic_cond","covid","glasses","BMI")

## ------------------------------------------------------------
## 2. Function to run pairwise mixed-effects models
## ------------------------------------------------------------

run_glmer_pairwise <- function(data, response_var, exposure_var, covars){

  lv <- levels(data[[response_var]])
  comb <- t(combn(lv, 2))

  all_results <- data.frame()
  models_out  <- list()

  for(i in seq_len(nrow(comb))){

    g1 <- comb[i,1]
    g2 <- comb[i,2]

    subdat <- data %>%
      filter(.data[[response_var]] %in% c(g1, g2)) %>%
      mutate(!!response_var := factor(.data[[response_var]], levels = c(g1, g2)))

    fml <- as.formula(
      paste0(response_var, " ~ ", paste(covars, collapse = " + "), " + (1|schoolID)")
    )

    fit <- glmer(
      fml,
      data = subdat,
      family = binomial(link = "logit"),
      control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5))
    )

    models_out[[paste0(g2, "_vs_", g1)]] <- fit

    ## pairwise emmeans for IA_status
    emm <- emmeans(fit, specs = exposure_var, type = "link")
    contr <- pairs(emm) %>% summary(infer=TRUE)

    contr_df <- as.data.frame(contr) %>%
      mutate(
        OR = exp(estimate),
        CI.low  = exp(estimate - 1.96*SE),
        CI.high = exp(estimate + 1.96*SE),
        comparison = paste0(g2, "_vs_", g1),
        variable = exposure_var
      )

    contr_df$p_adj_BH <- p.adjust(contr_df$p.value, method="BH")

    all_results <- bind_rows(all_results, contr_df)
  }

  return(list(
    models = models_out,
    pairwise = all_results
  ))
}

## ------------------------------------------------------------
## 3. Run analysis
## ------------------------------------------------------------

res_help <- run_glmer_pairwise(
  data,
  response_var = "help_status",
  exposure_var = "IA_status",
  covars = covariates
)

## ------------------------------------------------------------
## 4. Output results
## ------------------------------------------------------------

write.csv(res_help$pairwise, "output/05_helpseeking_pairwise.csv", row.names=FALSE)

## Extract full coefficients from each submodel (optional)
coef_all <- bind_rows(lapply(names(res_help$models), function(nm){

  tidy(res_help$models[[nm]], effects="fixed") %>%
    mutate(
      OR = exp(estimate),
      CI.low  = exp(estimate - 1.96*std.error),
      CI.high = exp(estimate + 1.96*std.error),
      comparison = nm
    )

}))

write.csv(coef_all, "output/04_helpseeking_model_coefs.csv", row.names=FALSE)

## End of script

