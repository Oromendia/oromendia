#' Print competing risks regression output with CI and P values
#'
#' This is a wrapper around broom:::tidy that adds formatted confidence intervals and p values
#' @param crr_mod Output from cmprsk
#' @param d Decimal places used for HR and confidence interval
#' @param d_pVal Decimal places used for p value.
#' @examples
#' \dontrun{
#' format_crr()
#' }
#' 
format_crr <- function(crr_mod,d=2,d_pVal=3){
  digi <- paste0("%.",d,"f")
  crr_coef = summary(crr_mod)$conf.int
  tab = cbind(
    Variable = gsub(".*\\~","",sapply(strsplit(rownames(crr_coef), ","), "[", 1)),
    HazardRatio = paste0(sprintf(digi,crr_coef[,"exp(coef)"])," (",
                         sprintf(digi,crr_coef[,"2.5%"]),",",
                         sprintf(digi,crr_coef[,"97.5%"]),")"),
    Pvalue = format_pval(summary(crr_mod)$coef[,"p-value"],equal="",d=d_pVal),
    N = summary(crr_mod)$n
  )
  rownames(tab) = NULL
  tab
}
