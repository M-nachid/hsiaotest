#' Classical Hsiao Homogeneity Test for Panel Data
#'
#' Conducts Hsiao's classical F-tests for homogeneity of intercepts and slopes across panel units.
#'
#' @description
#' This function implements a set of F-tests proposed by Hsiao (1986) to check for heterogeneity in panel data models.
#' It tests whether intercepts and slopes are constant across cross-sectional units (e.g., countries, firms).
#'
#' @param data A data frame containing the panel data.
#' @param dep_var Character string. Name of the dependent variable.
#' @param indep_vars Character vector. Names of the independent variables.
#' @param id_var Character string. Name of the individual (panel) identifier.
#' @param time_var Character string. Name of the time variable (optional, not used in the test).
#'
#' @return A data frame containing the test names, F-statistics, p-values, degrees of freedom, and decision results.
#'
#' @details
#' Three nested linear models are estimated:
#' \itemize{
#'   \item Model 1: Homogeneous intercepts and slopes
#'   \item Model 2: Heterogeneous intercepts only
#'   \item Model 3: Heterogeneous intercepts and slopes
#' }
#' Then, F-tests compare:
#' \enumerate{
#'   \item Model 1 vs Model 3: tests both intercept and slope homogeneity
#'   \item Model 2 vs Model 3: tests slope homogeneity
#'   \item Model 1 vs Model 2: tests intercept homogeneity
#' }
#'
#' @references
#' Hsiao, C. (1986). \emph{Analysis of Panel Data}. Cambridge University Press.
#'
#' @seealso [hsiao_test_robust()]
#'
#' @examples
#' \dontrun{
#' hsiao_test(
#'   data = Gasoline,
#'   dep_var = "lgaspcar",
#'   indep_vars = c("lincomep", "lrpmg", "lcarpcap"),
#'   id_var = "country",
#'   time_var = "year"
#' )
#' }
#'
#' @export
hsiao_test <- function(data, dep_var, indep_vars, id_var, time_var) {
  indep_str <- paste(indep_vars, collapse = " + ")

  formula_full <- as.formula(
    paste0(dep_var, " ~ ", indep_str, " + factor(", id_var, ") + ",
           paste0("factor(", id_var, "):", indep_vars, collapse = " + "))
  )

  formula_r1 <- as.formula(paste0(dep_var, " ~ ", indep_str))
  formula_r2 <- as.formula(paste0(dep_var, " ~ ", indep_str, " + factor(", id_var, ")"))

  mod_full <- lm(formula_full, data = data)
  mod_r1 <- lm(formula_r1, data = data)
  mod_r2 <- lm(formula_r2, data = data)

  test1 <- anova(mod_r1, mod_full)
  test2 <- anova(mod_r2, mod_full)
  test3 <- anova(mod_r1, mod_r2)

  dt <- data.frame(
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
  rempsyc::nice_table(dt)
}
