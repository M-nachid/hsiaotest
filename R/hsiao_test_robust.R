#' Robust Hsiao Homogeneity Tests
#'
#' Performs robust versions of Hsiao's tests for homogeneity of intercepts and slopes
#' across panel units, using heteroskedasticity-consistent standard errors.
#'
#' @param data A data frame
#' @param dep_var Name of the dependent variable (character)
#' @param indep_vars A character vector of independent variable names
#' @param id_var Name of the panel identifier (e.g., country)
#' @param time_var Name of the time variable (optional)
#'
#' @return A data frame with F-statistics, p-values, degrees of freedom, and conclusions
#'
#' @details
#' The function estimates three models:
#' (1) no heterogeneity, (2) heterogeneity in intercepts only, and (3) full heterogeneity.
#' It then performs F-tests comparing these models to assess homogeneity.
#'
#' @examples
#' \dontrun{
#' hsiao_test_robust(
#'   data = mydata,
#'   dep_var = "lgaspcar",
#'   indep_vars = c("lincomep", "lrpmg", "lcarpcap"),
#'   id_var = "country",
#'   time_var = "year"
#' )
#' }
#'
#' @export
hsiao_test_robust <- function(data, dep_var, indep_vars, id_var, time_var) {
  if (!requireNamespace("car", quietly = TRUE) ||
      !requireNamespace("sandwich", quietly = TRUE)) {
    stop("Packages 'car' and 'sandwich' must be installed.")
  }

  indep_str <- paste(indep_vars, collapse = " + ")

  # Full model (intercepts + slope heterogeneity)
  formula_full <- as.formula(
    paste0(dep_var, " ~ ", indep_str, " + factor(", id_var, ") + ",
           paste0("factor(", id_var, "):", indep_vars, collapse = " + "))
  )

  # Reduced models
  formula_r1 <- as.formula(paste0(dep_var, " ~ ", indep_str))
  formula_r2 <- as.formula(paste0(dep_var, " ~ ", indep_str, " + factor(", id_var, ")"))

  mod1 <- lm(formula_r1, data = data)
  mod2 <- lm(formula_full, data = data)
  mod3 <- lm(formula_r2, data = data)

  # Robust hypothesis tests
  t1 <- car::linearHypothesis(mod2,
                              car::matchCoefs(mod2, "factor\\("),
                              vcov = sandwich::vcovHC(mod2, type = "HC1"))

  t2 <- car::linearHypothesis(mod2,
                              car::matchCoefs(mod2, ":"),
                              vcov = sandwich::vcovHC(mod2, type = "HC1"))

  t3 <- car::linearHypothesis(mod3,
                              car::matchCoefs(mod3, "factor\\("),
                              vcov = sandwich::vcovHC(mod3, type = "HC1"))

  dt <- data.frame(
    Test = c("Homogeneity of slopes and intercepts",
             "Homogeneity of slopes",
             "Homogeneity of intercepts"),
    F.statistic = c(t1$F[2], t2$F[2], t3$F[2]),
    p.value = c(t1$`Pr(>F)`[2], t2$`Pr(>F)`[2], t3$`Pr(>F)`[2]),
    DF.diff = as.integer(c(t1$Df[2], t2$Df[2], t3$Df[2])),
    DF.unrestricted = as.integer(c(t1$Res.Df[2], t2$Res.Df[2], t3$Res.Df[2])),
    Result = c(
      ifelse(t1$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null"),
      ifelse(t2$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null"),
      ifelse(t3$`Pr(>F)`[2] < 0.05, "Reject null", "Fail to reject null")
    )
  )
  rempsyc::nice_table(dt)
}
