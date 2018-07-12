#' Print generic regression output with CI and P values
#'
#' This is a wrapper around broom:::tidy that adds formatted confidence intervals and p values
#' @param rslt Output from coxph(Surv(time,event~stat)) %>% tidy(exponentiate=T)
#' @param d Decimal places used for HR and confidence interval
#' @param d_pVal Decimal places used for p value.
#' @param sub_out String to from variable names to replace with sub_in
#' @param sub_in String to from variable names to replace instead of sub_out
#' @param exponentiate_here Select whether exponentiation should be done here
#' @keywords tidy
#' @examples
#' format_tidy_gen()
format_tidy_gen <- function(rslt,d=2,d_pVal=3,sub_out = NULL,sub_in = NULL,exponentiate_here = FALSE,Beta = "Beta",
	removeIntercept=TRUE){
  digi <- paste0("%.",d,"f")
  if(!is.null(sub_out)) {Variable = gsub(sub_out,sub_in,rslt$term)
  }else{Variable = rslt$term}
  rslt = cbind(
    Variable = Variable,
    Beta = if(exponentiate_here){
    	paste0(sprintf(digi,exp(rslt$estimate))," (",sprintf(digi,exp(rslt$conf.low)),",",sprintf(digi,exp(rslt$conf.high)),")")
    }else{paste0(sprintf(digi,rslt$estimate)," (",sprintf(digi,rslt$conf.low),",",sprintf(digi,rslt$conf.high),")")},
    Pvalue = format_pval(rslt$p.value,equal="",d=d_pVal)
  ) %>% as.data.frame()
  if(Beta != "Beta") colnames(rslt)[which(colnames(rslt) == "Beta")] = Beta
  if(removeIntercept) rslt = rslt[-which(rslt$Variable == "(Intercept)"),]
  rslt
}
