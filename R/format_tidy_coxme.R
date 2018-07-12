#' Print Cox Mixed effects regression output with CI and P values
#'
#' This is a wrapper around broom:::tidy that adds formatted confidence intervals and p values
#' @param crr_mod Output from coxme
#' @param d Decimal places used for HR and confidence interval
#' @param d_pVal Decimal places used for p value.
#' @param sub_out String to from variable names to replace with sub_in
#' @param sub_in String to from variable names to replace instead of sub_out
#' @examples
#' format_tidy_coxme()
format_tidy_coxme <- function(mod,d=2,d_pVal=3,sub_out = NULL,sub_in = NULL){
  digi <- paste0("%.",d,"f")
  if(!is.null(sub_out)) {Variable = gsub(sub_out,sub_in,names(mod$coef))
  }else{Variable = names(mod$coef)}
  n_here = mod$n[2]
  sd = sqrt(diag(vcov(mod)))
  logHR = mod$coef
  lowCI = logHR - 1.96*sd/sqrt(n_here)
  highCI = logHR + 1.96*sd/sqrt(n_here)
  tab = cbind(
    Variable = Variable,
    HazardRatio = paste0(sprintf(digi,exp(logHR))," (",sprintf(digi,exp(lowCI)),",",sprintf(digi,exp(highCI)),")"),
    Pvalue = format_pval((1-pnorm(abs(mod$coef/sd)))*2,equal="",d=d_pVal)
  )
  rownames(tab) = NULL
  return(tab)
}
