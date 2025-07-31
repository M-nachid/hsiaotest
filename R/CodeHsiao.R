

##################################################################
##               Importation of necessay packages               ##
##################################################################


# Import poobly  package to workspace
library(rempsyc)
library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(stringr)
library(poobly)
library(openxlsx)


#################################################################
##                   Importation of the data                   ##
#################################################################


# Import "Gasoline" dataset
Gasoline <- openxlsx::read.xlsx("C:/Users/moham/Desktop/Gasoline.xlsx")

# Print first 6 rows
head(Gasoline)



##################################################################
##                      Perform Hsiao test                      ##
##################################################################


df <- Gasoline
indep_vars <-  c('lincomep' , 'lrpmg' , 'lcarpcap')
indep_str <- paste(indep_vars, collapse = " + ")
dep_var <- 'lgaspcar'
id_var <- 'country'
time_var <- 'year'
formula_full <- as.formula(
  paste0(dep_var, " ~ ", indep_str, " + ",
         "factor(", id_var, ") + ",
         paste0("factor(", id_var, "):", indep_vars, collapse = " + "))
)
formula_full

formula_reduced1 <- as.formula(paste0(dep_var, " ~ ", indep_str))
formula_reduced2 <- as.formula(paste0(dep_var, " ~ ", indep_str, " + factor(", id_var, ")"))

# Fit models
model_full <- lm(formula_full, data = df)
model_r1 <- lm(formula_reduced1, data = df)
model_r2 <- lm(formula_reduced2, data = df)

# F-tests
test1 <- anova(model_r1, model_full) # test of slopes & intercepts
test2 <- anova(model_r2, model_full) # test of slopes
test3 <- anova(model_r1, model_r2)   # test of intercepts

out <- data.frame(
  Test = c("Homogeneity of slopes and intercepts",
           "Homogeneity of slopes",
           "Homogeneity of intercepts"),
  F.statistic = c(test1$F[2], test2$F[2], test3$F[2]),
  p.value = c(test1$`Pr(>F)`[2], test2$`Pr(>F)`[2], test3$`Pr(>F)`[2]),
  DF.diff = as.integer(c(test1$Df[2], test2$Df[2], test3$Df[2])),
  DF.unrestricted = as.integer(c(test1$Res.Df[2], test2$Res.Df[2], test3$Res.Df[2])),
  Result = c(
    ifelse(test1$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null"),
    ifelse(test2$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null"),
    ifelse(test3$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null")
  )
)
out

nice_table(out)


#################################################################
##                Interpretation of the results                ##
#################################################################


# Example test results (replace with your actual test output)
results <- data.frame(
  Test = c("Homogeneity of slopes and intercepts", "Homogeneity of slopes", "Homogeneity of intercepts"),
  p.value = c(test1$`Pr(>F)`[2], test2$`Pr(>F)`[2], test3$`Pr(>F)`[2])
)

cat("Interpretation of Test Results\n\n")

# Test 1
cat("Test 1: Homogeneity of slopes and intercepts\n")
cat("H0: α₁ = α₂ = ... = α_N and β₁ = β₂ = ... = β_N for all k\n")
if (!is.na(results$p.value[1]) && results$p.value[1] < 0.05) {
  cat("→ Result: Reject null hypothesis - Coefficients vary across cross-sections.\n\n")
} else {
  cat("→ Result: Fail to reject null - Coefficients are constant.\n\n")
}

# Test 2
cat("Test 2: Homogeneity of slopes\n")
cat("H0: β₁ = β₂ = ... = β_N for all k\n")
if (!is.na(results$p.value[2]) && results$p.value[2] < 0.05) {
  cat("→ Result: Reject null hypothesis - Slopes vary across cross-sections.\n\n")
} else {
  cat("→ Result: Fail to reject null - Slopes are constant.\n\n")
}

# Test 3
cat("Test 3: Homogeneity of intercepts\n")
cat("H0: α₁ = α₂ = ... = α_N\n")
if (!is.na(results$p.value[3]) && results$p.value[3] < 0.05) {
  cat("→ Result: Reject null hypothesis - Intercepts vary across cross-sections.\n\n")
} else {
  cat("→ Result: Fail to reject null - Intercepts are constant.\n\n")
}



##################################################################
##                         Robust Hsiao                         ##
##################################################################



if (!require("car")) install.packages("car")
library(car)

if (!require("sandwich")) install.packages("sandwich")
library(sandwich)


formula_full <- as.formula(
  paste0(dep_var, " ~ ", indep_str, " + ",
         "factor(", id_var, ") + ",
         paste0("factor(", id_var, "):", indep_vars, collapse = " + "))
)
formula_full

formula_reduced1 <- as.formula(paste0(dep_var, " ~ ", indep_str))
formula_reduced1
formula_reduced2 <- as.formula(paste0(dep_var, " ~ ", indep_str, " + factor(", id_var, ")"))
formula_reduced2

# Model 1: No heterogeneity
mod1 <- lm(formula_reduced1, data = df)

# Model 2: Full heterogeneity (intercepts + slope interactions)
mod2 <- lm(formula_full, data = df)

# Model 3: Intercept heterogeneity only
mod3 <- lm(formula_reduced2, data = df)


#Test 1: Homogeneity of slopes and intercepts
##(test if all coefficients are the same across countries)
t1 <- linearHypothesis(mod2,
                       matchCoefs(mod2, "factor\\(country\\)"),
                       vcov = vcovHC(mod2, type = "HC1"))

#Test 2: Homogeneity of slopes
##(test only interaction terms)
t2 <- linearHypothesis(mod2,
                       matchCoefs(mod2, ":"),
                       vcov = vcovHC(mod2, type = "HC1"))

#Test 3: Homogeneity of intercepts
##(test only the factor(country) terms)


t3 <- linearHypothesis(mod3,
                       matchCoefs(mod3, "factor\\(country\\)"),
                       vcov = vcovHC(mod3, type = "HC1"))

dt <- data.frame(
  Test = c("Homogeneity of slopes and intercepts", "Homogeneity of slopes", "Homogeneity of intercepts"),
  F.statistic = c(t1$F[2], t2$F[2], t3$F[2]),
  p.value = c(t1$`Pr(>F)`[2], t2$`Pr(>F)`[2], t3$`Pr(>F)`[2]),
  DF.diff = as.integer(c(t1$Df[2], t2$Df[2], t3$Df[2])),
  DF.unrestricted = as.integer(c(t1$Res.Df[2], t2$Res.Df[2], t3$Res.Df[2])),
  Result = c(
    ifelse(test1$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null"),
    ifelse(test2$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null"),
    ifelse(test3$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null")
  )
)
dt

